#!/usr/bin/env stack
-- stack runghc --package reanimate --package linear --package reanimate-svg
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Graphics.SvgTree.Types
import           Linear.V2              (V2 (V2))
import           Reanimate
import           Reanimate.Animation

main :: IO ()
main = reanimate $ bg `parA` thales

bg :: Animation
bg = staticFrame 0 $ mkBackground "white"

-- Step duration
sd :: Duration
sd = 5

thales :: Animation
thales =
  sceneAnimation $ do
    play $ mkAnimation 1 (drawCircle)
    play $
      signalA (bellS 3) $
      mkAnimation sd $
      (\angleT ->
         mkGroup
           [ drawCircle 1
           , drawTriangle angleT
           , translate 5 0 $ drawWedges angleT
           ])

r :: Double
r = 4

dot :: SVG
dot = mkCircle 0.04

drawCircle :: Time -> SVG
drawCircle lineT =
  env $ mkGroup [mkCircle r, draw lineT $ mkLine (-r, 0) (r, 0)]

drawTriangle :: Time -> SVG
drawTriangle angleT =
  let radiusAngle = pi * angleT
      x = r * cos radiusAngle
      y = r * sin radiusAngle
      triangle = mkLinePath [(-r, 0), (x, y), (r, 0), (-r, 0)]
      wedges =
        withClipPathRef (Ref "clip") $
        mkGroup
          [ mkWedge "green" (-r, 0) (radiusAngle / 2) 0
          , mkWedge "green" (x, y) (radiusAngle / 2) (pi + radiusAngle / 2)
          , mkWedge
              "red"
              (r, 0)
              ((pi - radiusAngle) / 2)
              ((pi + radiusAngle) / 2)
          , mkWedge "red" (x, y) ((pi - radiusAngle) / 2) (pi + radiusAngle)
          ]
   in env $
      mkGroup
        [ mkClipPath "clip" [triangle]
        , wedges
        , triangle # withFillOpacity 0
        , mkLine (0, 0) (x, y)
        , dot
        , translate x y dot
        , translate (-r) 0 dot
        , translate r 0 dot
        -- Square coinciding with the triangle
        , mkGroup [mkRect 8 8, dot] # withFillOpacity 0 #
          translate (x + 4) (y - 4) #
          rotateAround (90 * angleT - 90) (V2 x y)
        ]

drawWedges :: Time -> SVG
drawWedges angleT =
  let greenAngle = pi * angleT / 2
      redAngle = pi / 2 - greenAngle
   in mkGroup
        [ mkWedge "green" (0, 0) greenAngle 0
        , mkWedge "red" (0, 0) redAngle greenAngle
        ]

-- TODO: get rid of wedges and maybe just draw circles
mkWedge :: String -> (Double, Double) -> Double -> Double -> SVG
mkWedge color (cx, cy) wedgeAngle rotAngle =
  let r = 0.5
      x = r * cos wedgeAngle
      y = r * sin wedgeAngle
      rotAngleDeg = rotAngle / pi * 180
   in mkPath
        [ MoveTo OriginAbsolute [V2 0 0]
        , LineTo OriginAbsolute [V2 r 0]
        , EllipticalArc OriginAbsolute [(r, r, 0, False, True, V2 x y)]
        , EndPath
        ] #
      rotate rotAngleDeg #
      translate cx cy #
      withFillColor color #
      withStrokeWidth 0

draw :: Time -> SVG -> SVG
draw t = partialSvg t . pathify

env :: SVG -> SVG
env =
  withFillColor "white" .
  withStrokeColor "black" . withStrokeWidth (defaultStrokeWidth / 2)
