#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree (Number(..))
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal
import           Reanimate.Raster
import           Reanimate.Povray
import           Codec.Picture
import           Data.String.Here

main :: IO ()
main = reanimate $ pauseAtEnd 2 $ mkAnimation 5 $ do
    s <- getSignal $ signalFromTo 0 360 signalLinear
    n <- getSignal $ signalLinear
    emit $ mkBackground "black"
    emit $ povrayMedium ["+A"] (script s)
    emit $
      withFillColor "white" $
      withFillOpacity n $
      mkCircle (Num 40.4)
  where
    script s = [iTrim|
//EXAMPLE OF SPHERE

//Files with predefined colors and textures
#include "colors.inc"
#include "glass.inc"
#include "golds.inc"
#include "metals.inc"
#include "stones.inc"
#include "woods.inc"

#include "shapes3.inc"

//Place the camera
camera {
  orthographic
  // angle 50
  location <0,0,-10>
  look_at  <0,0,0>
  //right x*image_width/image_height
  up <0,9,0>
  right <16,0,0>
}


//Ambient light to "brighten up" darker pictures
global_settings { ambient_light White*10 }

//Set a background color
//background { color White }
//background { color rgbt <0.1, 0, 0, 0> } // red
background { color rgbt <0, 0, 0, 1> } // transparent

//intersection
//{
//Sphere with specified center point and radius
sphere {
  <0,0,0>, 2.00
  texture {
    pigment{ color rgbf <0,0,1,1> }
  }
}

object {
  Ring_Sphere(2.00, 2.02, 0.015, 0.015, 12, 12)
  texture {
    pigment{ color<1,1,1> }
  }
  rotate <0,${round s::Int},0>
  rotate <-30,0,0>
}
//}

             |]
