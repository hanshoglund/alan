
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

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
module Alan where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Supply
import Data.Version
import System.Process


-- API

type GhcFilePath        = FilePath
type CabalFilePath      = FilePath
type StageDirFilePath = FilePath
type AlanError          = String

type Value = Int -- TODO JSON
type Persistent = Int -- TODO JSON

-- The running procs are restricted to this type
data AlanProc = forall s . AlanProc (Maybe Persistent -> Value -> s -> (Maybe Persistent, Value, s))

newtype Alan a = Alan
  (ReaderT (GhcFilePath, CabalFilePath, StageDirFilePath)
    (ExceptT AlanError IO)
    a)


-- Server is started with
runAlan
    :: GhcFilePath
    -> CabalFilePath
    -> StageDirFilePath
    -> Alan a
    -> IO (Either String a)
runAlan = undefined
-- runAlan = runErrorT

data Stage -- JSON
data Performer -- JSON

type PackageDescr = [(String,Version)]
type SourceTree = [(FilePath,String)]

try :: Alan a -> Alan (Either AlanError a)
addStage :: [PackageDescr] -> Alan Stage
-- setup a package db
start :: Stage -> SourceTree -> Alan Performer
-- compile/eval code, using package DBs from Stage
send :: Performer -> Value -> Alan Value
[try,addStage,start,send] = undefined


-- IMPL

-- Implementation 1: GHC/Cabal sandbox
-- Implementation 2: GHC/Cabal stack
-- Implementation 3: Hint/Cabal sanbox
-- Implementation 4: GHCJS




-- TEST

alan1 :: AlanProc
alan1 = AlanProc $ \_ n s -> (Nothing, n+s, n+s)




--
-- POST /Stagees
--   [(package name,version)] -> Alan Stage
--       creates Stage
--       installs packages
-- POST /start
--   Stage -> [(source file tree,main file)] -> Alan Performer
--     compiles and launces a Performer
--     passed source MUST define a AlanProc value as "main" (IO not accepted)
-- POST /send
--   Performer -> [Value] -> Alan [Value]
