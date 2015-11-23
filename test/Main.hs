
module Main where

import Alan
import Data.Version
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Except (liftIO)
import qualified Data.List
import qualified Data.Char


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
    let deps = [("music-pitch-literal", makeVersion[1,9,0])]
    s <- addStage deps
    pf <- start s [
      ("Alpha.hs",  "module Alpha where beta = 'b'XX"),
      ("Beta.hs",   "module Beta where import Data.IORef; import Music.Pitch.Literal; beta = 'b'"),
      ("Main.hs",   "module Main where import Music.Pitch.Literal; import Alpha; import Beta(beta); main = writeFile \"Output.txt\" (show (take 201 $ repeat $ (fs::Int)) ++ \"\\nDone!\\n\") ")]
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
    Left e  -> print $ "Alan test error: " ++ e
