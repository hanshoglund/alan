
{-# LANGUAGE RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving, TypeFamilies #-}

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

  AlanConfiguration(..),
  defAlanConfiguration,

  AlanServer,
  runAlanServer,

  Package,
  SourceTree,
  Dependencies(..),

  Stage,
  Performer,
  addStage,
  start,
  send,

  ParseMessage(..),
  Queue,
  receive,

  ) where

{-
Design notes:

  Server:

  - Written as a library wrapped in a web server
  - Does NOT specify how the code is actually executed (as long as it is correct)
  - The core library is in a monad AlanServer
  - At startup, the AlanServer computation claims a single Alan directory
    COROLLARY
      Multiple AlanServer's can be run in parallel using different directories
  - If the AlanServer monad fails, performers are killed, and the Alan directory is relinquished
    COROLLARY
      Users typically want to use catchError (or @try x = catchError (fmap Right x) (return . Left)@) to prevent this
  - The AlanServer acts as a front-end to a single system of stages and performers
      - It maintains integrity of the directories (and thus the state of mangaged packages, compiler artifacts etc).
      - It can support parallel building of instances and possibly Stages (depending on whether Stack supports this).
      COROLLARY
        AlanServer calls can not block.
        Sending messages should be fast.
        Recieving messages can be poll-based.
          Alternatively:
            - Some kind of promise/queue/observer pattern.

  Performers:
    - Compiled modules can NOT access the IO monad. This is enforced as follows:
      - Safe Haskell is used to compile user code
      - User code is not allowed to define @Main.main@ (in safe Haskell, the only way to run an IO computation is to be invoked by main)
      - Instead, they define Main.alanMain, which is invoked by a main function generated by alan.

    - What type is Main.alanMain? It should be something simple that does not require user code to import Alan.
    - Libraries can be written to facilitate use of this type later on (i.e. an Alan) monad.


  Longer (older) version
  ----------------------------------------------------------------------

  Write as a library (Alan.hs)
    Wrap as a HTTP server (Alan/Server.hs) and client (Alan/Client.hs)
    Top-level execitable alan (Alan/Main.hs) invokes either
    Keep top-level API simple (< 5 functions)

    All persistant state is stored in the Alan directory
    No  processes spawned by Alan are daemons, so no process leaks are possible


    The server runs in a single Alan directory, where all the stages (housing dependency related information, typically package-dbs)
    and performers (housing everything related to a running process, including sources and binaries) are stored.
      The Alan directory should NOT be shared between server processes.
      All persistant state is stored in the Alan directory. Note that the AlanServer monad is conceptually effect-free,
      so anything created in the Alan directory should be for caching/optimization purposes and not affect behavior at all.
    The AlanServer monad is sequential but not thread-safe.
      COROLLARY
        Any invocation of AlanServer methods from server a wrapper must be queued.

    The API is deliberately vague about HOW the submitted code is executed, as part of point of Alan is to hide system-specific details.

    There are serveral implementation choices:
      - Is the code compiled or interpreted?
      - Are package databases used, or some other linking strategy (i.e. package mangling, interpreting everything).
      - What compiler/interpreter is used (GHC-n, GHCJS, other?).
      - How are package databases (if used) created (stack, cabal-install, ghc-pkg, manually)
      - How are the processes launched?
      - How does communication between the processes and Alan (hence the world) happen. File handles, sockets, runtime linking,
        shared memory etc.

    How to communicate with a running performer?

      - Again performers *may* (and usually will) run in a separate process.
        COROLLARY We *must* have  a binary protocol to talk to them
        - In this we need to handle both encoding/decoding and actual I/O
        - Whatever API we choose for I/O it will be implemnted in terms of System.IO.h[Get|Put]Buf*, so no need to worry about line mode/locale/encoding.
        - By default, we will use the standard in/out Handles. Possible alternatives for sharing bigger data:
          - POSIX shared memory (packages: unix, shared-memory)
        - While the underlying transport handles byte sequences, we usually want to have a message API on top of this (so Performers can be stream processors
          of custom types, rather than stream processors of bytes). Of course we could just define performers as (Byte -> s -> (s, Byte)), but it makes
          sense to provide some basic incremental parsing on top.
        - Examples of incremental parsing:
            Line parsing: (a message is emitted whenever \n is encountered, regardless of context)
            Escaped line parsing: (a message is emitted whenever an '\n', not preceded by '\\' is encountered, regardless of context)
            Attoparsec (Aeson/AttoLisp): A message is emitted un successful parse. A successful or failed parse will restart the parser (so parsers should allow junk at the beginning).
            Cereal (Putter/Get)

    - Base API (on the server side)
      - Sends raw byte sequences (in the AlanServer monad)
      - Provides polling recv in AlanServer monad for simpicity
        - Alternative: there is a separate thread blocking on each incoming handle (fits well with attoparsec etc).
-}

import qualified Data.ByteString
import Data.ByteString (ByteString)

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (asum)
import Data.Version
import System.Process
import Data.Monoid
import qualified Control.Exception
import qualified System.IO
import qualified Data.Aeson
import Data.Aeson (ToJSON, FromJSON)
import qualified Numeric
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as LBS
import qualified System.Directory
import qualified System.Process
import qualified Data.Maybe
import qualified Data.Map as Map
import qualified Data.List

import Alan.ParseEnv

import qualified System.Environment
import qualified System.FilePath.Posix as FilePath

import System.Exit (ExitCode(..))


-- API

type AlanServerError = String
type AlanServerLogged = String

data AlanConfiguration  = AlanConfiguration {
  -- At the moment, each Alan server requires a preinstalled GHC and Cabal
  -- Might be changed
  alanConfGhcExecutable   :: FilePath,      -- defaults to the value of (which ghc)
  alanConfCabalExecutable :: FilePath,      -- defaults to the value of (which cabal)
  alanConfStackExecutable :: FilePath,      -- defaults to the value of (which stack)
  alanConfAlanDirectory   :: Maybe FilePath -- defaults to ~/.alan
  }
-- TODO in monad to suppot the actual paths above
defAlanConfiguration      = AlanConfiguration {
  alanConfGhcExecutable   = "/usr/bin/ghc",
  alanConfCabalExecutable = "/usr/bin/cabal",
  alanConfStackExecutable = "stack",
  alanConfAlanDirectory   = Nothing
  }


type Message    = String -- TODO JSON
type Persistent = String -- TODO JSON

newtype AlanState = AlanState { alanStateConf :: AlanConfiguration }
-- Store handler to which log msgs are sent
-- Store ProcessHandle for each performer we have spawned, indexed by performed id
-- Store I/O handles

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

data Performer = Performer
  String -- 128-bit hash
  -- Int    -- Instance id
  deriving (Show)

type Package    = (String, Version) -- I.e. [("aeson", fromString "0.10.0.0")]
type SourceTree = [(FilePath, String)] -- I.e. [("Main.hs","module Main where alan = ...")]

-- | All packages (as would be specified in a Cabal file), with a compatible Stack resolver.

-- Current implementation (always ignores resolver):
  -- Cabal: Any system level package + the ones specified here
  -- Stack: Packages in default resolver + the ones specified here
data Dependencies = Dependencies { getResolver :: String, getDependencies :: [Package] }

-- | Create a new stage.
addStage :: Dependencies -> AlanServer Stage
addStage dependencies = do
  let overwrite = False

  -- Generate ID
  let stageId = hashJson $ (fmap (fmap show) (getDependencies dependencies), getResolver dependencies)

  -- Compute relevant paths
  homeDir  <- liftIOWithException $ System.Directory.getHomeDirectory
  alanDir  <- fmap (Data.Maybe.fromMaybe (homeDir ++ "/.alan") . alanConfAlanDirectory . alanStateConf) ask
  cabalExe <- fmap (alanConfCabalExecutable . alanStateConf) ask
  ghcExe   <- fmap (alanConfGhcExecutable . alanStateConf) ask
  let stageDir = alanDir ++ "/" ++ stageId

  -- Assure stage directory
  there <- liftIOWithException $ System.Directory.doesDirectoryExist stageDir
  when (there && overwrite) $ liftIOWithException $ System.Directory.removeDirectoryRecursive stageDir

  -- Create the stage
  -- unless there $ addStageCabal stageDir dependencies
  -- FIXME on failure stage is considered to exist
  unless there $ addStageStack stageDir dependencies
  return $ Stage stageId


-- | Start a new performer using the given stage.
start :: Stage -> SourceTree -> AlanServer Performer
start (Stage stageId) sources = do

  -- Generate ID
  let performerId = hashJson $ (sources, stageId)

  -- Compute relevant paths
  homeDir  <- liftIOWithException $ System.Directory.getHomeDirectory
  alanDir  <- fmap (Data.Maybe.fromMaybe (homeDir ++ "/.alan") . alanConfAlanDirectory . alanStateConf) ask

  -- Generate performer id (stageId+unique Message)
  let stageDir     = alanDir ++ "/" ++ stageId
  let performerDir = alanDir ++ "/performers/" ++ performerId

  -- Assure performer directory
  there <- liftIOWithException $ System.Directory.doesDirectoryExist performerDir
  unless there $ liftIOWithException $ System.Directory.createDirectoryIfMissing True performerDir

  -- Note: If the performer directory existed, files should exist too, but rewrite in case a file was accidentally removed
  -- Note that GHC won't recompile unless checksums are different
  writeFilesInDirectory performerDir sources

  -- launchProcessCabal stageDir performerDir
  launchProcessStack stageDir performerDir

  return $ Performer performerId


-- | Send a message to the given perfomer.
send :: Performer -> Message -> AlanServer ()
  -- Write to input
  -- Block waiting for output


-- newtype ParseMessage fail a = PM (forall r . ((fail -> r) -> (a -> r) -> ((ByteString -> ParseMessage fail a) -> r) -> r))

newtype ParseMessage a = PM
  (Either (ByteString -> ParseMessage a) (ByteString, a))
newtype Queue a = Q a

receive :: Performer -> (ByteString -> ParseMessage a) -> AlanServer (Queue a)
[send, receive] = undefined


-- Attoparsec example
-- resultToPm r = case r of { Partial f -> (PM $ Left (\bs -> resultToPm $ f bs)) ; Done _ x -> PM (Right (Right x)) ; Fail _ _ e -> PM (Right (Left e)) }
-- receiveAtto parser = receive (\bs -> resultToPm (parse parser bs))

-- IMPLEMENTATION

-- | Write all files in the given directory. Can handle subpaths but not patterns like .., . or ~.
writeFilesInDirectory :: FilePath -> SourceTree -> AlanServer ()
writeFilesInDirectory dir sources = do
  forM_ sources $ \(path,code) -> do
    liftIOWithException $ System.Directory.createDirectoryIfMissing True (FilePath.takeDirectory (dir ++ "/" ++ path))
    liftIOWithException $ System.IO.writeFile (dir ++ "/" ++ path) code
    return ()


-- | Prepare the given stage directory with the given dependencies.
addStageCabal :: FilePath -> Dependencies -> AlanServer ()
addStageCabal stageDir dependencies = do
  -- TODO does not work if (null dependencies == True)
  cabalExe <- fmap (alanConfCabalExecutable . alanStateConf) ask
  cabalEnv <- liftIOWithException $ inheritSpecifically ["HOME"]

  liftIOWithException $ System.Directory.createDirectoryIfMissing True stageDir
  (_,_,_,p) <- liftIOWithException $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir, env = cabalEnv })
    $ System.Process.proc cabalExe ["sandbox", "init", "--sandbox", stageDir ++ "/sb"]
  r <- liftIOWithException $ System.Process.waitForProcess p
  case r of
    ExitSuccess -> return ()
    ExitFailure e -> throwError $ cabalExe ++ " exited with code: " ++ show e
  return ()

  (_,_,_,p) <- liftIOWithException $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir, env = cabalEnv })
    $ System.Process.proc cabalExe (["-j", "install"]
      ++ fmap (\(name,version) -> name ++ "-" ++ showVersion version)
      (getDependencies dependencies))
  r <- liftIOWithException $ System.Process.waitForProcess p
  case r of
    ExitSuccess -> return ()
    ExitFailure e -> throwError $ cabalExe ++ " exited with code: " ++ show e
  return ()

-- | Start a performer, assuming
--    * That the given stage directory has been prepared by a call to addStageXX
--    * That the given performer directory contains all *sources*, including Main.hs, defining Main.main.
launchProcessCabal :: FilePath -> FilePath -> AlanServer ()
launchProcessCabal stageDir performerDir = do
  -- TODO replace arch/OS/GHC version here by parsing cabal.sandbox.config and looking at package-db: field
  let packDbDir    = stageDir ++ "/sb/x86_64-osx-ghc-7.10.2-packages.conf.d"

  ghcExe <- fmap (alanConfGhcExecutable . alanStateConf) ask
  ghcEnv <- liftIOWithException $ inheritSpecifically ["HOME"]

  (_,_,_,p) <- liftIOWithException $ System.Process.createProcess $ (\x -> x { cwd = Just performerDir, env = ghcEnv }) $
    System.Process.proc ghcExe [
      "-package-db=" ++ packDbDir,
      "-threaded",
      "-O2",
      "-XSafe",
      "-i" ++ performerDir, -- or .
      "--make", "Main.hs",
      "-o", performerDir ++ "/AlanMain"
      ]
  r <- liftIOWithException $ System.Process.waitForProcess p
  case r of
    ExitSuccess -> return ()
    ExitFailure e -> throwError $ ghcExe ++ " exited with code: " ++ show e
  (_,_,_,p) <- liftIOWithException $ System.Process.createProcess $ (\x -> x { cwd = Just performerDir, env = emptyEnvironment }) $
    System.Process.proc (performerDir ++ "/AlanMain") []

  -- TODO handle termination
  return ()

-- Currently Cabal/GHC assumes global conf, and Stack assumes lts-3.15
addStageStack :: FilePath -> Dependencies -> AlanServer ()
addStageStack stageDir dependencies = do

  let resolver         = "lts-3.15"
  let cabalPackageSpec = Data.List.intercalate ",\n" $
        (fmap (\(name,version) -> "        " ++ name ++ " ==" ++ showVersion version) $ getDependencies dependencies)
  let stackPackageSpec = Data.List.intercalate "\n" $
        -- TODO should remove all packages present in the resolver
        -- (though Stack tolerates them)
        (fmap (\(name,version) -> "  - " ++ name ++ "-" ++ showVersion version) $ getDependencies dependencies)

  -- Create dummy Setup.hs, a.cabal, Main.hs and stack.yaml
  writeFilesInDirectory stageDir [
        ("Setup.hs", "import Distribution.Simple\n"
                  ++ "main = defaultMain\n"),
        ("Main.hs",  "main = return ()\n"),
        ("a.cabal", "cabal-version: >= 1.2\n"
                  ++ "name:         a\n"
                  ++ "version:      0.0.0.1\n"
                  ++ "build-type:   Simple\n"
                  ++ "\n"
                  ++ "executable AlanDummy\n"
                  ++ "    main-is: Main.hs\n"
                  ++ "    hs-source-dirs: .\n"
                  ++ "    build-depends:\n"
                  ++ cabalPackageSpec),
        ("stack.yaml",
                     "resolver: " ++ resolver ++ "\n"
                  ++ "extra-deps:\n"
                  ++ stackPackageSpec)
                ]

  stackExe <- fmap (alanConfStackExecutable . alanStateConf) ask
  stackEnv <- liftIOWithException $ inheritSpecifically ["HOME", "PATH"]

  -- Run stack build --install-ghc
  (_,_,_,p) <- liftIOWithException $ System.Process.createProcess $ (\x -> x { cwd = Just stageDir, env = stackEnv }) $
    System.Process.proc stackExe ["build", "--install-ghc", "--no-system-ghc"]
  r <- liftIOWithException $ System.Process.waitForProcess p
  case r of
    ExitSuccess -> return ()
    ExitFailure e -> throwError $ stackExe ++ " exited with code: " ++ show e


  -- Run stack exec env
  (r,out,err) <- liftIOWithException $ flip System.Process.readCreateProcessWithExitCode "" $ (\x -> x { cwd = Just stageDir, env = stackEnv }) $
    System.Process.proc stackExe ["--no-system-ghc", "exec", "/usr/bin/env"]
  case r of
    ExitSuccess -> return ()
    ExitFailure e -> throwError $ stackExe ++ " exited with code " ++ show e ++ " and message " ++ err

  writeFilesInDirectory stageDir [("ENV-DEBUG", out)]
  (compiler, packDbs) <- getCompilerAndPackagePathFromEnv out
  writeFilesInDirectory stageDir [
        ("COMPILER", compiler),
        ("PACKAGE_DBS", unlines packDbs)
        ]
  return ()

-- getCompilerAndPackagePathFromEnv :: String -> AlanServer (String, [String])
-- getCompilerAndPackagePathFromEnv str = case runParser par str of
--   Right (Res (First (Just compiler), First (Just packDbs))) -> return (compiler, packDbs)
--   Right x -> throwError $ "Could not parse env: strange result: " ++ show x
--   Left e -> throwError $ "Could not parse env: " ++ e
--   where
--     ls :: [String]
--     ls = lines str
--
-- newtype Res = Res (First String, First [String]) -- compiler, packDbs
--   deriving (Show, Monoid)
--
-- par :: Parser Res
-- par = fmap mconcat $ P.sepEndBy1 (asum [
--   P.try $ fmap (findCompilerPath) pathParser,
--   P.try $ fmap (\dbs -> Res (mempty,First (Just dbs))) packDbParser,
--   P.try $ eatLine
--   ]) (CP.string "\n")
--   where
--     eatLine = P.many (CP.noneOf "\n") >> return mempty
--
-- findCompilerPath xs = case Data.List.find (".stack/programs" `Data.List.isInfixOf`) xs of
--   Nothing -> mempty
--   Just x  -> Res (First (Just x),mempty)
--
-- -- PATH line, separated by :, the one containing .stack/programs/
-- pathParser :: Parser [String]
-- pathParser = do
--   CP.string "PATH="
--   r <- P.sepEndBy (P.many (asum [CP.alphaNum, CP.oneOf "_-./"])) (CP.char ':')
--   return r
--
-- -- GHC_PACKAGE_PATH, separated by :
-- packDbParser :: Parser [String]
-- packDbParser = do
--   CP.string "GHC_PACKAGE_PATH="
--   r <- P.sepEndBy (P.many (asum [CP.alphaNum, CP.oneOf "_-./"])) (CP.char ':')
--   return r
--
-- -- MonadPlus, Parsing, CharParsing
-- -- type Parser = RP.ReadP
-- runParser :: Parser a -> String -> Either String a
-- -- runParser x input = case RP.readP_to_S x input of
-- --   []          -> Left "ReadP failed"
-- --   ((x,_) : _) -> Right x
-- type Parser = Parsec.Parsec String ()
-- runParser x input = case Parsec.runParser x () "Noname" input of
--   Left e -> Left (show e)
--   Right x -> Right x

launchProcessStack :: FilePath -> FilePath -> AlanServer ()
launchProcessStack stageDir performerDir = do
  dbPaths <- fmap lines $ liftIOWithException $ Prelude.readFile (stageDir ++ "/PACKAGE_DBS")
  ghcExe <- fmap (++ "/ghc") $ liftIOWithException $ Prelude.readFile (stageDir ++ "/COMPILER")
  ghcEnv <- liftIOWithException $ inheritSpecifically ["HOME", "PATH"]

  (_,_,_,p) <- liftIOWithException $ System.Process.createProcess $ (\x -> x { cwd = Just performerDir, env = ghcEnv }) $
    System.Process.proc ghcExe (fmap ("-package-db="++) dbPaths ++ [
      "-threaded",
      "-O2",
      "-XSafe",
      -- "-v", -- TODO
      "-i" ++ performerDir, -- or .
      "--make", "Main.hs",
      "-o", performerDir ++ "/AlanMain"
      ])
  r <- liftIOWithException $ System.Process.waitForProcess p
  case r of
    ExitSuccess -> return ()
    ExitFailure e -> throwError $ ghcExe ++ " exited with code: " ++ show e
  (_,_,_,p) <- liftIOWithException $ System.Process.createProcess $ (\x -> x { cwd = Just performerDir, env = emptyEnvironment }) $
    System.Process.proc (performerDir ++ "/AlanMain") []

  -- TODO handle termination
  -- Invoke GHC
  return ()

    {-
      Stack implementation:
        Add stage:

          Create dummy Setup.hs, a.cabal, Main.hs and stack.yaml

          Setup.hs
  import Distribution.Simple
  main = defaultMain

            a.cabal
  cabal-version: >= 1.2
  name: a
  version: 0.0.0.1
  build-type: Simple

  executable AlanDummy
    main-is: Main.hs
    hs-source-dirs: .
    build-depends:
      -- Exact package versions, i.e.
      base,
      Boolean ==0.2.3,
      reverse-apply ==2.0.1

            Main.hs
  main = return ()

          stack.yaml
  resolver: lts-2.14 # resolver, if any
  extra-deps:
    - reverse-apply-2.0.1 # all packages not in resolver

          Then do
            stack build --install-ghc

        Start:
         ~/.stack/programs/x86_64-linux/ghc-7.8.4/bin/ghc \
          -package-db=/home/hans/.stack/snapshots/x86_64-linux/lts-2.14/7.8.4/pkgdb \
          -package-db=/home/hans/.alan/STAGE_ID/.stack-work/install/x86_64-linux/lts-2.14/7.8.4/pkgdb \
          --make -o AlanMain2 Main.hs

          (We can get both GHC path and package DBs from running stack exec env in stage dir)
          hans@bigtom:~/.alan/STAGE_ID$ stack exec env
          PATH=/home/hans/.stack/programs/x86_64-linux/ghc-7.8.4/bin
          GHC_PACKAGE_PATH=
            /home/hans/.alan/STAGE_ID/.stack-work/install/x86_64-linux/lts-2.14/7.8.4/pkgdb
            :
            /home/hans/.stack/snapshots/x86_64-linux/lts-2.14/7.8.4/pkgdb
            :
            /home/hans/.stack/programs/x86_64-linux/ghc-7.8.4/lib/ghc-7.8.4/package.conf.d

    -}

    -- Instead of the sandbox, create a dummy stack project (generate stack.yaml and a dummy library if needed)
    -- When compiling, concatenate GHC pack-db, lts pack-db (in .stack directory), and pack-db in stage.




-- -- The running procs are restricted to this type
-- data AlanProc = forall s . AlanProc (Maybe Persistent -> (s, Message -> s -> (Maybe Persistent, Maybe Message, s)))
-- -- TODO Al monad
-- -- runAl Al () -> AlanProc
-- alanId :: AlanProc
-- alanId = AlanProc $ \_ -> ((), \m () -> (Nothing, Just m, ()))
--
-- -- TODO remove Main from incoming code and add something like this
-- alanMain :: AlanProc -> IO ()
-- alanMain (AlanProc startup) = do
--   -- Messages are lines to stdin/stdout (TODO escape newlines, or even use fancy binary modes)
--   -- Persistance not implemented (requires identiciation of processes as per above)
--   let (initState, update) = startup Nothing
--   recur update initState
--   where
--     recur update state = do
--         msg <- System.IO.getLine
--         let (_, mOut, state') = update msg state
--         -- send message
--         case mOut of
--           Nothing -> return ()
--           Just msg -> System.IO.putStrLn msg
--         recur update state'

-- UTIL

inheritCompleteEnvironment :: Maybe [(String, String)]
inheritCompleteEnvironment = Nothing

emptyEnvironment :: Maybe [(String, String)]
emptyEnvironment = Just []

-- | Create an environment inheriting exactly the given properties from the system environment (the environment used to invoke alan).
inheritSpecifically :: [String] -> IO (Maybe [(String, String)])
inheritSpecifically ks = do
  base <- fmap Map.fromList $ System.Environment.getEnvironment
  return $ Just $ Map.toList (appAll (fmap (\k -> case (Map.lookup k base) of { Just v -> Map.insert k v ; Nothing -> id }) ks) Map.empty)
    where
      appAll = Prelude.foldr (.) id

overwriteEnvironment :: String -> String -> IO (Maybe [(String, String)])
overwriteEnvironment k v = do
  base <- fmap Map.fromList $ System.Environment.getEnvironment
  return $ Just $ Map.toList (Map.insert k v base)

-- | Hash any object with a JSON representation
hashJson :: ToJSON a => a -> String
hashJson = (=<<) (twoChars . ($ "") . Numeric.showHex) . LBS.unpack .  toHash
  where
        toHash = MD5.hashlazy . Data.Aeson.encode
        twoChars [a   ] = ['0', a]
        twoChars [a, b] = [a,   b]

liftIOWithException :: IO a -> AlanServer a
liftIOWithException k = liftIO (Control.Exception.try k) >>= \x -> case x of
  Left e  -> throwError $ show (e :: Control.Exception.SomeException)
  Right x -> return x



-- Primitives

data M a
instance Functor M
instance Applicative M
instance Monad M
fork        :: M () -> M () -- todo block, stop

data R
data W
data Var rw a
instance (rw ~ R) => Functor (Var rw)
instance (rw ~ R) => Applicative (Var rw)
data Chan rw a
instance (rw ~ R) => Functor (Chan rw)
-- Contravariant (Chan W)

newChan     :: M (Chan R a, Chan W a)
dupChan     :: Chan R a -> M (Chan R a)
readChan    :: Chan R a -> M a
tryReadChan :: Chan R a -> M (Maybe a)
writeChan   :: Chan W a -> a -> M ()

newVar   :: a       -> M (Var R a, Var W a)
readVar  :: Var R a -> M a
writeVar :: Var W a -> a -> M ()
[newChan, dupChan, readChan, tryReadChan, fork, writeChan, dupVar, newVar, readVar, writeVar] = undefined


-- FRP interface

-- This implementation requires 'fork/ in the base monad.
-- An alternative could be to use race/concurrently from "async":
--   appendE: race
--   scatter: race
--   accum, snapshot, run: ?

type Event a = M (Chan R a)
memptyE :: Event a
memptyE = fmap fst newChan
appendE :: Event a -> Event a -> Event a
appendE a b = do
  x <- a
  y <- b
  z <- newChan
  fork (forever $ readChan x >>= writeChan (snd z))
  fork (forever $ readChan y >>= writeChan (snd z))
  return (fst z)
-- trivial functor
scatterE :: Foldable t => Event (t a) -> Event a
scatterE a = do
  x <- a
  z <- newChan
  fork $ forever $ do
    vs <- readChan x
    forM_ vs (writeChan (snd z))
  return (fst z)
type Beh a = M (Var R a)
-- trivial functor, applicative?

mapE :: (a -> b) -> Event a -> Event b
mapE f = fmap (fmap f)

foldpR :: (a -> b -> b) -> b -> Event a -> Beh b
foldpR f z e = accumR z (mapE f e)

accumR :: a -> Event (a -> a) -> Beh a
accumR z e = do
  e' <- e
  v  <- newVar z
  fork $ forever $ do
    f <- readChan e'
    r <- readVar (fst v)
    writeVar (snd v) (f r)
    return ()
  return $ fst v

snapshotWith :: (a -> b -> c) -> Beh a -> Event b -> Event c
snapshotWith f b e = do
  b' <- b
  e' <- e
  z <- newChan
  fork $ forever $ do
    bv <- readVar b'
    ev <- readChan e'
    writeChan (snd z) (f bv ev)
  return (fst z)

-- Launch a thread that wakes up on input, runs it through the FRP network, calls the output sink
runR :: (Event a -> Event b) -> M a -> (b -> M ()) -> M ()
runR f inp outp = do
  x <- newChan
  fork (forever $ inp >>= writeChan (snd x))
  outpCh <- f (dupChan $ fst x)
  forever $ readChan outpCh >>= outp
  return ()

-- runR2 :: (Event a -> Event b) -> (M a, a -> M ())
