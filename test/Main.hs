
module Main where

import Alan
import Data.Version
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Except (liftIO)

main = do
  runAlan defAlanConfiguration $ do
    s <- addStage False []
    start s [
      ("Alpha.hs",  "module Alpha where beta = 'b'"),
      ("Beta.hs",   "module Beta where beta = 'b'"),
      ("Main.hs",   "module Main where import Beta(beta); main = writeFile \"Output.txt\" \"Done!\"")]
    -- addStage False [("music-pitch-literal", makeVersion[1,9,0])]
    -- addStage False [
        -- ("music-pitch-literal", makeVersion[1,9,0])
      -- , ("music-pitch", makeVersion[1,9,0])
      -- ]
    forever $ liftIO $ threadDelay 1000
    return ()
  print "Ok"
