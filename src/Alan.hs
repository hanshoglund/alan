
{-# LANGUAGE RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

{-
Alan is a HTTP server that compiles/interprets Haskell code.
Code generates a server (called Performers) which can be interacted with through the parent Alan server.
Every performer runs in a sandbox (called Stage), and is isolated from other performers.

Alan performers can peform arbitrary computation, but I/O is restricted to
  - sending and recieving messages
  - altering a local state
  - (optionally)

The Safe Haskell restriction (which essentially forbids unsafePerformIO) is used to enforce this.


Usage:
  alan serve --port 8675 &

  alan add-stage mystage
  alan start --url localhost --port 8675 mystage -I . Main.hs
    Launches a new service, prints process ID.
  alan send localhost a2b4c1b2 {"command":"status"}

Implementation:
  Needs to:
    Compile/Eval code
    Download code from Hackage/Stackage
    Create a package DB (ghc-pkg, poss. with stack or cabal-install)



-}
module Alan (
  AlanError,
  Message,
  Persistent,
  AlanProc(..),
  AlanConfiguration(..),
  defAlanConfiguration,
  Alan,
  runAlan,
  Stage,
  Performer,
  Package,
  SourceTree,
  try,
  addStage,
  start,
  send
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Supply
import Data.Version
import System.Process
import qualified System.IO

-- For hash
import qualified Data.Aeson
import Data.Aeson (ToJSON, FromJSON)
import qualified Numeric
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString as LBS
import qualified System.Directory
import qualified System.Process
import qualified Data.Maybe


-- API

type AlanError          = String
data AlanConfiguration  = AlanConfiguration {
  -- At the moment, each Alan server requires a preinstalled GHC and Cabal
  -- Might be changed
  alanConfGhcExecutable :: FilePath,
  alanConfCabal         :: FilePath,
  alanConfAlanDirectory :: Maybe FilePath
  }
defAlanConfiguration = AlanConfiguration {
  alanConfGhcExecutable = "ghc",
  alanConfCabal         = "cabal",
  alanConfAlanDirectory = Nothing
  }


type Message = String -- TODO JSON
type Persistent = String -- TODO JSON

-- The running procs are restricted to this type
data AlanProc = forall s . AlanProc (Maybe Persistent -> (s, Message -> s -> (Maybe Persistent, Maybe Message, s)))
-- TODO Al monad
-- runAl Al () -> AlanProc
alanId :: AlanProc
alanId = AlanProc $ \_ -> ((), \m () -> (Nothing, Just m, ()))

-- All persistant state is stored in the Alan directory
-- All processes spawned by Alan are non-daemons, so no process leaks are possible
newtype Alan a = Alan
  (ReaderT AlanConfiguration
    (ExceptT AlanError IO)
    a)
    deriving (Functor, Applicative, Monad, MonadError AlanError, MonadIO, MonadReader AlanConfiguration)

-- Server is started with
runAlan
    :: AlanConfiguration
    -> Alan a
    -> IO (Either String a)
runAlan conf (Alan x) = runExceptT (runReaderT x conf)

data Stage = Stage String -- Must be JSONable
  deriving (Show)
data Performer = Performer String -- Must be JSONable
  deriving (Show)

type Package    = (String, Version) -- I.e. [("aeson", fromString "0.10.0.0")]
type SourceTree = [(FilePath, String)] -- I.e. [("Main.hs","module Main where alan = ...")]

try :: Alan a -> Alan (Either AlanError a)

-- TODO addStage could create races if multiple alans are run on the same dir (A is building a stage, B obverve it as already existing)
-- TODO start has similar problem
-- For now, document that only one instance of alan should be run over an alan directory (they can run over different directories though)
-- Eventually lock on PID or similar (i.e. alan server writes its PID to the alan dir before operating and refuses to proceed
-- if a process with that PID exists on the system)

addStage :: Bool -> [Package] -> Alan Stage
addStage overwrite dependencies = do
  -- Generate stage ID (hash of deps)
  let stageId = hashJson $ (fmap (fmap show) dependencies)
  homeDir <- liftIO $ System.Directory.getHomeDirectory
  alanDir <- fmap (Data.Maybe.fromMaybe (homeDir ++ "/.alan") . alanConfAlanDirectory) ask
  let stageDir = alanDir ++ "/" ++ stageId

  there <- liftIO $ System.Directory.doesDirectoryExist stageDir
  when (there && overwrite) $ liftIO $ System.Directory.removeDirectoryRecursive stageDir

  unless there $ do
    liftIO $ System.Directory.createDirectoryIfMissing True stageDir
    -- TODO cabal path
    (_,_,_,p) <- liftIO $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir }) $ System.Process.proc "cabal" ["sandbox", "init",
      "--sandbox", stageDir ++ "/sandbox"]
    liftIO $ System.Process.waitForProcess p
    return ()

  forM_ dependencies $ \(name,version) -> do
    let x = name ++ "-" ++ showVersion version
    (_,_,_,p) <- liftIO $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir }) $ System.Process.proc "cabal" ["install", x]
    liftIO $ System.Process.waitForProcess p
    return ()

  return $ Stage stageId
    -- If Bool is true, wipe out preexisting stage and restart
    -- Create stage if not existing
      -- Go to SB dir
      -- <alanDir>/stages/a12b2246
      -- Run cabal sandbox init
      -- Run cabal sandbox install [packages]
    -- Return Stage (with id)

start :: Stage -> SourceTree -> Alan Performer
start (Stage stageId) sources = do
  let performerId = hashJson $ (sources,stageId)
  homeDir <- liftIO $ System.Directory.getHomeDirectory
  alanDir <- fmap (Data.Maybe.fromMaybe (homeDir ++ "/.alan") . alanConfAlanDirectory) ask
  -- Generate performer id (stageId+unique Message)
  let stageDir     = alanDir ++ "/" ++ stageId
  -- TODO replace arch/OS/GHC version here by parsing cabal.sandbox.config and looking at package-db: field
  let packDbDir    = stageDir ++ "/sandbox/x86_64-osx-ghc-7.10.2-packages.conf.d"
  let performerDir = alanDir ++ "/performers/" ++ performerId

  -- TODO verify paths
  -- TODO ghc path
  -- TODO assumes Main.hs exists
  there <- liftIO $ System.Directory.doesDirectoryExist performerDir
  unless there $ do
    liftIO $ System.Directory.createDirectoryIfMissing True performerDir
    return ()

  -- TODO always write files for now
  forM_ sources $ \(path,code) -> do
    -- TODO assure subdirs!
    liftIO $ System.IO.writeFile (performerDir ++ "/" ++ path) code
    return ()

  -- TODO always recompile for now
  -- TODO compile errors seem to be bounced back to Alan Process
  -- is this always the case? If so: catch
  (_,_,_,p) <- liftIO $ System.Process.createProcess $ (\x -> x { cwd = Just performerDir }) $
    System.Process.proc "ghc" [
      "-package-db=" ++ packDbDir,
      "-threaded",
      "-O2",
      "-i"++performerDir, -- or .
      "--make", "Main.hs",
      "-o", performerDir ++ "/AlanMain"
      ]
  liftIO $ System.Process.waitForProcess p
  (_,_,_,p) <- liftIO $ System.Process.createProcess $ (\x -> x { cwd = Just performerDir }) $
    System.Process.proc (performerDir ++ "/AlanMain") []

  -- Go to perf dir, place sources here
    -- <alanDir>/performers/<performerId>
  -- Add wrapping alanMain
  -- Compile or interpret this code with stage pack-db, launch result
  -- Performers are NOT daemons

-- compile/eval code, using package DBs from Stage
  return $ Performer performerId


send :: Performer -> Message -> Alan Message
  -- Write to input
  -- Block waiting for output

[try,send] = undefined

-- TODO this has to be added to incoming SourceTrees
alanMain :: AlanProc -> IO ()
alanMain (AlanProc startup) = do
  -- Messages are lines to stdin/stdout (TODO escape newlines, or even use fancy binary modes)
  -- Persistance not implemented (requires identiciation of processes as per above)
  let (initState, update) = startup Nothing
  recur update initState
  where
    recur update state = do
        msg <- System.IO.getLine
        let (_, mOut, state') = update msg state
        -- send message
        case mOut of
          Nothing -> return ()
          Just msg -> System.IO.putStrLn msg
        recur update state'

-- IMPL

-- Implementation 1: GHC/Cabal sandbox


-- Implementation 2: GHC/Cabal stack
-- Implementation 3: Hint/Cabal sanbox
-- Implementation 4: GHCJS




-- TEST

-- alan1 :: AlanProc
-- alan1 = AlanProc $ \_ n (Just s) -> (Nothing, n+s, n+s)


-- UTIL

-- | Hash any object with a JSON representation
hashJson :: ToJSON a => a -> String
hashJson = (=<<) (twoChars . ($ "") . Numeric.showHex) . LBS.unpack .  toHash
  where
        toHash = MD5.hashlazy . Data.Aeson.encode
        twoChars [a   ] = ['0', a]
        twoChars [a, b] = [a,   b]




--
-- POST /Stagees
--   [(package name,version)] -> Alan Stage
--       creates Stage
--       installs packages
-- POST /start
--   Stage -> [(source file tree,main file)] -> Alan Performer
--     compiles and launces a Performer
--     passed source MUST define a AlanProc Message as "main" (IO not accepted)
-- POST /send
--   Performer -> [Message] -> Alan [Message]
