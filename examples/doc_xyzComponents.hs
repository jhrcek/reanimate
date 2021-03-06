#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Interpolate
import Reanimate.Builtin.Documentation
import Codec.Picture

main :: IO ()
main = reanimate $ docEnv $ animate $ const $ showColorMap $ interpolateRGB8 xyzComponents yellow blue
  where
    yellow = PixelRGB8 0xFF 0xFF 0x00
    blue = PixelRGB8 0x00 0x00 0xFF
