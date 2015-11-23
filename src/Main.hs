
module Main where

import Alan ()
import Options.Applicative
import Web.Scotty

main = do
  print "alan!"
  -- serve

  -- start
  -- add-stage
  -- send



-- REST:
  -- POST stages
    -- {dependencies:[{name:"...", version: "0.1.0"},...]}
  -- GET  stages
  -- GET  stages/:id
  -- POST performers
    -- {stage:id, sources:[{path:"...", code:"..."},...]}
  -- GET performers
  -- GET performers/:id
  -- POST performers/:id/messages
      -- Posts response directly, or need separate request?
