
{-
Alan is a server that compiles and launches Haskell services using GHC and Cabal.

Every service is sandboxed and runs as a separate process. Services are restricted to send and recieve JSON messages and
alter service-specific JSON state.

Use Safe Haskell by default.

-}
module Alan where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Supply
import Data.Version

type GhcFilePath        = FilePath
type CabalFilePath      = FilePath
type SandboxDirFilePath = FilePath
type AlanError          = String

type Value = () -- TODO JSON
type State = Value

-- The running procs are restricted to this type
type AlanProc = Value -> State -> (Value, State)


newtype Alan a = Alan
  (ReaderT (GhcFilePath, CabalFilePath, SandboxDirFilePath)
    (ExceptT AlanError IO)
    a)


-- Server is started with
runAlan
    :: GhcFilePath
    -> CabalFilePathx
    -> SandboxDirFilePath
    -> Alan a
    -> IO (Either String a)
runAlan = undefined
-- runAlan = runErrorT

data Sandbox -- JSON
data Process -- JSON

type PackageDescr = [(String,Version)]
type SourceTree = [(FilePath,String)]

try :: Alan a -> Alan (Either AlanError a)
addSandbox :: [PackageDescr] -> Alan Sandbox
start :: Sandbox -> SourceTree -> Alan Process
send :: Process -> Value -> Alan Value

[try,addSandbox,start,send] = undefined
--
-- POST /sandboxes
--   [(package name,version)] -> Alan Sandbox
--       creates sandbox
--       installs packages
-- POST /start
--   Sandbox -> [(source file tree,main file)] -> Alan Process
--     compiles and launces a process
--     passed source MUST define a AlanProc value as "main" (IO not accepted)
-- POST /send
--   Process -> [Value] -> Alan [Value]
