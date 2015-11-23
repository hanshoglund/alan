
{-# LANGUAGE RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

{-|
Alan is a HTTP/REST server and library that compiles and executes Haskell code on-the-fly.

Alan leverages Stack to support multiple language environments (called /stages/),
which can be configured to run a specified /dialect/ of the Haskell language, by
specifying GHC version, compiler options and packages.

Users invoke Alan by submitting a set of Haskell modules, which are immediately
type-checked and launched. The resultant process is called a /performer/.

Each performer runs into a sandboxed environment and can not access memory, files,
environment of other performers, or the underlying system. Otherwise they can
perform arbitrary computation and respond to requests via the Alan system that
launched them.

The Safe Haskell mode is used to enforce this:
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe-haskell.html>

@
Usage:
  alan serve --port 8675 &

  alan add-stage mystage
  alan start --url localhost --port 8675 mystage -I . Main.hs
    Launches a new service, prints process ID.
  alan send localhost a2b4c1b2 {"command":"status"}
@
-}
module Alan (
  AlanServerError,
  Message,
  Persistent,
  AlanProc(..),
  AlanConfiguration(..),
  defAlanConfiguration,
  AlanServer,
  runAlanServer,
  Stage,
  Performer,
  Package,
  SourceTree,
  try,
  addStage,
  start,
  send
  ) where

{-
Design notes:
  Write as a library (Alan.hs)
    Wrap as a HTTP server (Alan/Server.hs) and client (Alan/Client.hs)
    Top-level execitable alan (Alan/Main.hs) invokes either
    Keep top-level API simple (< 5 functions)

    Compiled modules can NOT access the IO monad. This is enforced as follows:
      Safe Haskell is used to compile user code
      User code is not allowed to define @Main.main@ (in safe Haskell, the only way to run an IO computation is to be invoked by main)
      Instead, they define Main.alanMain, which is invoked by a main function generated by alan.

    What type is Main.alanMain? It should be something simple that does not require user code to import Alan.
    Libraries can be written to facilitate use of this type later on (i.e. an Alan) monad.

    The server runs in a single Alan directory, where all the stages (housing package-dbs) and performers (housing sources and artefacts) are stored.
      The Alan directory should NOT be shared between server processes.
      All persistant state is stored in the Alan directory. Note that the AlanServer monad is conceptually effect-free,
      so anything created in the Alan directory should be for caching/optimization purposes and not affect behavior at all.
    The AlanServer monad is sequential but not thread-safe.
      COROLLARY
        Any invocation of AlanServer methods from server wrappers must be queued.

    The API is deliberately vague about HOW the submitted code is executed, as part of point of Alan is to hide system-specific details.

    There are serveral implementation choices:
      - Is the code compiled or interpreted?
      - Are package databases used, or some other linking strategy (i.e. package mangling, interpreting everything).
      - What compiler/interpreter is used (GHC-n, GHCJS, other?).
      - How are package databases (if used) created (stack, cabal-install, ghc-pkg, manually)
      - How are the processes launched?
      - How does communication between the processes and Alan (hence the world) happen. File handles, sockets, runtime linking,
        shared memory etc.
-}

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

type AlanServerError          = String
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

newtype AlanState = AlanState { alanStateConf :: AlanConfiguration }

-- All persistant state is stored in the Alan directory
-- All processes spawned by Alan are non-daemons, so no process leaks are possible
newtype AlanServer a = AlanServer
  (ReaderT AlanState
    (ExceptT AlanServerError IO)
    a)
    deriving (Functor, Applicative, Monad, MonadError AlanServerError, MonadIO, MonadReader AlanState)

-- Server is started with
runAlanServer
    :: AlanConfiguration
    -> AlanServer a
    -> IO (Either String a)
runAlanServer conf (AlanServer x) = runExceptT (runReaderT x (AlanState conf))

data Stage = Stage String -- Must be JSONable
  deriving (Show)
data Performer = Performer String -- Must be JSONable
  deriving (Show)

type Package    = (String, Version) -- I.e. [("aeson", fromString "0.10.0.0")]
type SourceTree = [(FilePath, String)] -- I.e. [("Main.hs","module Main where alan = ...")]

try :: AlanServer a -> AlanServer (Either AlanServerError a)

-- TODO addStage could create races if multiple alans are run on the same dir (A is building a stage, B obverve it as already existing)
-- TODO start has similar problem
-- For now, document that only one instance of alan should be run over an alan directory (they can run over different directories though)
-- Eventually lock on PID or similar (i.e. alan server writes its PID to the alan dir before operating and refuses to proceed
-- if a process with that PID exists on the system)

-- | Create a new stage.
addStage :: [Package] -> AlanServer Stage
addStage dependencies = do
  let overwrite = True

  -- Generate stage ID (hash of deps)
  let stageId = hashJson $ (fmap (fmap show) dependencies)
  homeDir <- liftIO $ System.Directory.getHomeDirectory
  alanDir <- fmap (Data.Maybe.fromMaybe (homeDir ++ "/.alan") . alanConfAlanDirectory . alanStateConf) ask
  let stageDir = alanDir ++ "/" ++ stageId

  there <- liftIO $ System.Directory.doesDirectoryExist stageDir
  when (there && overwrite) $ liftIO $ System.Directory.removeDirectoryRecursive stageDir

  -- TODO Stack implementation
  -- Instead of the sandbox, create a dummy stack project (generate stack.yaml and a dummy library if needed)
  -- When compiling, concatenate GHC pack-db, lts pack-db (in .stack directory), and pack-db in stage.

  unless there $ do
    liftIO $ System.Directory.createDirectoryIfMissing True stageDir
    -- TODO cabal path
    (_,_,_,p) <- liftIO $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir }) $ System.Process.proc "cabal" ["sandbox", "init",
      "--sandbox", stageDir ++ "/sb"]
    liftIO $ System.Process.waitForProcess p
    return ()

  -- forM_ dependencies $ \(name,version) -> do
  --   let x = name ++ "-" ++ showVersion version
  --   (_,_,_,p) <- liftIO $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir }) $ System.Process.proc "cabal" ["install", x
  --     -- ,
  --     -- "--sandbox", stageDir ++ "/sandbox"
  --     ]
  --   liftIO $ System.Process.waitForProcess p
  --   return ()
    (_,_,_,p) <- liftIO $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir }) $ System.Process.proc "cabal" (["-j", "install"]
      ++ fmap (\(name,version) -> name ++ "-" ++ showVersion version) dependencies)
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

-- | Start a new performer using the given stage.
start :: Stage -> SourceTree -> AlanServer Performer
start (Stage stageId) sources = do
  let performerId = hashJson $ (sources,stageId)
  homeDir <- liftIO $ System.Directory.getHomeDirectory
  alanDir <- fmap (Data.Maybe.fromMaybe (homeDir ++ "/.alan") . alanConfAlanDirectory . alanStateConf) ask
  -- Generate performer id (stageId+unique Message)
  let stageDir     = alanDir ++ "/" ++ stageId
  -- TODO replace arch/OS/GHC version here by parsing cabal.sandbox.config and looking at package-db: field
  let packDbDir    = stageDir ++ "/sb/x86_64-osx-ghc-7.10.2-packages.conf.d"
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
      "-XSafe",
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


-- | Send a message to the given perfomer.
send :: Performer -> Message -> AlanServer ()
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
