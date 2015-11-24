
module Main where

import Alan
import Data.Version
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Except (liftIO)
import qualified Data.List
import qualified Data.Char
import qualified System.Exit


readDep :: String -> (String, Version)
readDep str = (packagePart, Version (readVersion versionPart) [])
  where
    readVersion :: String -> [Int]
    readVersion [] = []
    readVersion xs =
      let part1 = Data.List.takeWhile (\x -> Data.Char.isDigit x) xs in
        read part1 : readVersion (Data.List.dropWhile (== '.') $ drop (length part1) xs)
    packagePart = dropAtEnd (length versionPart + 1) str
    versionPart = takeWhileAtEnd (\x -> Data.Char.isDigit x || x == '.') str
    takeWhileAtEnd p = reverse . Data.List.takeWhile p . reverse
    dropAtEnd n = reverse . drop n . reverse

readDeps :: FilePath -> IO [(String,Version)]
readDeps path = do
  str <- readFile path
  return $ map readDep (lines str)

main = do
  alanRes <- runAlanServer defAlanConfiguration $ do
    -- deps <- liftIO $ readDeps "/Users/Hoglund/suite/DEPS"

    -- let deps = [("music-pitch-literal", makeVersion[1,9,0])]
    let deps = Dependencies "XXX" [("reverse-apply", makeVersion[2,0,1])]

    s <- addStage deps
    pf <- start s [
      ("Foo/Alpha.hs",  "module Foo.Alpha where beta = 'b'"),
      ("Beta.hs",       "module Beta where import Data.IORef; beta = 'b'"),
      ("Main.hs",       "module Main where import Foo.Alpha; import Beta(beta); main = writeFile \"Output.txt\" (show (take 2001 $ repeat $ (22::Int)) ++ \"\\nDone!\\n\") ")]
    -- addStage False [("music-pitch-literal", makeVersion[1,9,0])]
    -- addStage False [
        -- ("music-pitch-literal", makeVersion[1,9,0])
      -- , ("music-pitch", makeVersion[1,9,0])
      -- ]
    liftIO $ print pf
    forever $ liftIO $ threadDelay 1000
    return ()
  case alanRes of
    Right _ -> print "Ok"
    Left e  -> System.Exit.die $ "Alan test error: " ++ e
