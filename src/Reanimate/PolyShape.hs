module Reanimate.PolyShape
  ( PolyShape(..)
  , PolyShapeWithHoles(..)
  , svgToPolyShapes     -- :: Tree -> [PolyShape]

  , renderPolyShape     -- :: PolyShape -> Tree
  , renderPolyShapes    -- :: [PolyShape] -> Tree
  , renderPolyShapePoints -- :: PolyShape -> Tree

  , plPathCommands      -- :: PolyShape -> [PathCommand]
  , plLineCommands      -- :: PolyShape -> [LineCommand]

  , plLength            -- :: PolyShape -> Double
  , plCurves            -- :: PolyShape -> [CubicBezier Double]
  , isInsideOf          -- :: PolyShape -> PolyShape -> Bool

  , plFromPolygon       -- :: [RPoint] -> PolyShape
  , plPolygonify        -- :: Double -> PolyShape -> [Point Double]
  , plDecompose         -- :: [PolyShape] -> [[RPoint]]
  , unionPolyShapes     -- :: [PolyShape] -> [PolyShape]
  , plDecompose'        -- :: Double -> [PolyShape] -> [[RPoint]]
  , decomposePolygon    -- :: [Point Double] -> [[RPoint]]
  , plGroupShapes       -- :: [PolyShape] -> [PolyShapeWithHoles]
  , mergePolyShapeHoles -- :: PolyShapeWithHoles -> PolyShape
  ) where

import           Chiphunk.Low
import           Control.Lens        ((&), (.~))
import           Data.List           (nub, partition, sortBy)
import           Data.Ord
import           Debug.Trace
import           Geom2D.CubicBezier  (ClosedPath (..), CubicBezier (..), DPoint,
                                      FillRule (..), PathJoin (..), Point (..),
                                      arcLength, bezierIntersection,
                                      closedPathCurves, closest, colinear,
                                      curvesToClosed, evalBezier, splitBezier,
                                      union, vectorDistance)
import           Graphics.SvgTree    (PathCommand (..), RPoint, Tree (..),
                                      defaultSvg, pathDefinition, Number(Num))
import           Linear.V2
import           Reanimate.Constants
import           Reanimate.Svg

-- | Shape drawn by continuous line. May have overlap, may be convex.
newtype PolyShape = PolyShape { unPolyShape :: ClosedPath Double }
  deriving (Show)

data PolyShapeWithHoles = PolyShapeWithHoles
  { polyShapeParent :: PolyShape
  , polyShapeHoles  :: [PolyShape]
  }


renderPolyShapes :: [PolyShape] -> Tree
renderPolyShapes pls =
  PathTree $ defaultSvg & pathDefinition .~ concatMap plPathCommands pls

renderPolyShape :: PolyShape -> Tree
renderPolyShape pl =
    PathTree $ defaultSvg & pathDefinition .~ plPathCommands pl

renderPolyShapePoints :: PolyShape -> Tree
renderPolyShapePoints = mkGroup . map renderPoint . plCurves
  where
    renderPoint (CubicBezier (Point x y) _ _ _) =
      translate x y $ mkCircle (Num 0.02)

plLength :: PolyShape -> Double
plLength = sum . map cubicLength . plCurves
  where
    cubicLength c = arcLength c 1 polyShapeTolerance

-- 1/10th of a pixel if rendered at 2560x1440
polyShapeTolerance :: Double
polyShapeTolerance = screenWidth/25600

plFromPolygon :: [RPoint] -> PolyShape
plFromPolygon = PolyShape . ClosedPath . map worker
  where
    worker (V2 x y) = (Point x y, JoinLine)

-- | Deconstruct a polyshape into non-intersecting, convex polygons.
plDecompose :: [PolyShape] -> [[RPoint]]
plDecompose = plDecompose' 0.001

-- | Deconstruct a polyshape into non-intersecting, convex polygons.
plDecompose' :: Double -> [PolyShape] -> [[RPoint]]
plDecompose' tol =
  concatMap decomposePolygon .
  map (plPolygonify tol) .
  map mergePolyShapeHoles .
  plGroupShapes .
  unionPolyShapes

decomposePolygon :: [Point Double] -> [[RPoint]]
decomposePolygon poly =
  map (map fromVect) $ convexDecomposition (map toVect poly) tol
  where
    tol = polyShapeTolerance
    toVect (Point x y) = Vect x y
    fromVect (Vect x y) = V2 x y

plPolygonify :: Double -> PolyShape -> [Point Double]
plPolygonify tol shape =
    startPoint (head curves) : concatMap worker curves
  where
    curves = plCurves shape
    worker c | endPoint c == startPoint c =
      [] -- error $ "Bad bezier: " ++ show c
    worker c =
      if colinear c tol
        then [endPoint c]
        else
          let (lhs,rhs) = splitBezier c 0.5
          in worker lhs ++ worker rhs
    endPoint (CubicBezier _ _ _ d) = d
    startPoint (CubicBezier a _ _ _) = a


plPathCommands :: PolyShape -> [PathCommand]
plPathCommands = lineToPath . plLineCommands

plLineCommands :: PolyShape -> [LineCommand]
plLineCommands pl =
  case curves of
    []                  -> []
    (CubicBezier start _ _ _:_) ->
      LineMove (toRPoint start) :
      map worker curves ++
      [LineEnd (toRPoint start)]
  where
    curves = plCurves pl
    worker (CubicBezier _ b c d) =
      LineBezier (map toRPoint [b,c,d])
    toRPoint :: Point Double -> RPoint
    toRPoint (Point x y) = V2 x y

svgToPolyShapes :: Tree -> [PolyShape]
svgToPolyShapes = cmdsToPolyShapes . toLineCommands . extractPath

cmdsToPolyShapes :: [LineCommand] -> [PolyShape]
cmdsToPolyShapes [] = []
cmdsToPolyShapes cmds =
    case cmds of
      (LineMove dst:cont) -> map PolyShape $ worker dst [] cont
      _                   -> bad
  where
    bad = error $ "Reanimate.PolyShape: Invalid commands: " ++ show cmds
    finalize [] rest  = rest
    finalize acc rest = (ClosedPath $ reverse acc) : rest
    worker _from acc [] = finalize acc []
    worker _from acc (LineMove newStart : xs) =
      finalize acc $
      worker newStart [] xs
    worker from acc (LineEnd orig:LineMove dst:xs) | from /= orig =
      finalize ((toGPoint from, JoinLine):acc) $
      worker dst [] xs
    worker _from acc (LineEnd{}:LineMove dst:xs) =
      finalize (acc) $
      worker dst [] xs
    worker from acc [LineEnd orig] | from /= orig =
      finalize ((toGPoint from, JoinLine):acc) []
    worker _from acc [LineEnd{}] =
      finalize (acc) []
    worker from acc (LineBezier [x]:xs) =
      worker x ((toGPoint from, JoinLine) : acc) xs
    worker from acc (LineBezier [a,b,c]:xs) =
      worker c ((toGPoint from, JoinCurve (toGPoint a) (toGPoint b)) : acc) xs
    worker _ _ _ = bad

    toGPoint :: RPoint -> Point Double
    toGPoint (V2 x y) = Point x y

unionPolyShapes :: [PolyShape] -> [PolyShape]
unionPolyShapes shapes =
    map PolyShape $
    union (map unPolyShape shapes) NonZero polyShapeTolerance

-- True iff lhs is inside of rhs.
-- lhs and rhs may not overlap.
-- Implementation: Trace a vertical line through the origin of A and check
-- of this line intersects and odd number of times on both sides of A.
isInsideOf :: PolyShape -> PolyShape -> Bool
lhs `isInsideOf` rhs =
    odd (length upHits) && odd (length downHits)
  where
    (upHits, downHits) = polyIntersections origin rhs
    origin = polyShapeOrigin lhs

polyIntersections :: DPoint -> PolyShape -> ([DPoint],[DPoint])
polyIntersections origin rhs =
    (nub $ concatMap (intersections rayUp) curves
    ,nub $ concatMap (intersections rayDown) curves)
  where
    curves = plCurves rhs

    intersections line bs =
      map (evalBezier bs . fst) (bezierIntersection bs line polyShapeTolerance)
    limit = 1000
    rayUp = CubicBezier origin origin origin (Point limit limit)
    rayDown = CubicBezier origin origin origin (Point (-limit) (-limit))

polyShapeOrigin :: PolyShape -> Point Double
polyShapeOrigin (PolyShape closedPath) =
  case closedPath of
    ClosedPath []            -> Point 0 0
    ClosedPath ((start,_):_) -> start

plGroupShapes :: [PolyShape] -> [PolyShapeWithHoles]
plGroupShapes = worker
  where
    worker (s:rest)
      | null (parents s rest) =
        let isOnlyChild x = parents x (s:rest) == [s]
            (holes, nonHoles) = partition isOnlyChild rest
            prime = PolyShapeWithHoles
              { polyShapeParent = s
              , polyShapeHoles  = holes }
        in prime : worker nonHoles
      | otherwise = trace ("Found hole, putting back") $ worker (rest ++ [s])
    worker [] = []

    parents :: PolyShape -> [PolyShape] -> [PolyShape]
    parents self = filter (self `isInsideOf`) . filter (/=self)

instance Eq PolyShape where
  a == b = plCurves a == plCurves b

mergePolyShapeHoles :: PolyShapeWithHoles -> PolyShape
mergePolyShapeHoles (PolyShapeWithHoles parent []) = parent
mergePolyShapeHoles (PolyShapeWithHoles parent (child:children)) =
  mergePolyShapeHoles $
    PolyShapeWithHoles (mergePolyShapeHole parent child) children

-- Merge
mergePolyShapeHole :: PolyShape -> PolyShape -> PolyShape
mergePolyShapeHole parent child =
  snd $ head $
  sortBy (comparing fst)
  [ cutSingleHole newParent child
  | newParent <- polyShapePermutations parent ]

{-
parent:
  (a,b)
  (b,c)
  (c,a)

child:
  (x,y)
  (y,z)
  (z,x)

P = split (a,b)
new:
  (P,b) p2b
  (b,c) pTail
  (c,a) pTail
  (a,P) a2p

  (P,x) p2x

  (x,y) childCurves
  (y,z) childCurves
  (z,x) childCurves

  (x,P) x2p

-}
cutSingleHole :: PolyShape -> PolyShape -> (Double, PolyShape)
cutSingleHole parent child =
    (score, PolyShape $ curvesToClosed $
      p2b:pTail ++ [a2p] ++
      [p2x] ++ childCurves ++
      [x2p]
    )
  where
    score = vectorDistance childOrigin p
    childOrigin = polyShapeOrigin child
    (pHead:pTail) = plCurves parent
    childCurves = plCurves child

    pParam = closest pHead childOrigin polyShapeTolerance

    (a2p, p2b) = splitBezier pHead pParam

    p = evalBezier pHead pParam
    -- straight line to child origin
    p2x = lineBetween p childOrigin
    -- straight line from child origin
    x2p = lineBetween childOrigin p

    lineBetween a b = CubicBezier a a a b

plCurves :: PolyShape -> [CubicBezier Double]
plCurves = closedPathCurves . unPolyShape

polyShapePermutations :: PolyShape -> [PolyShape]
polyShapePermutations =
    map (PolyShape . curvesToClosed) . cycleList . plCurves
  where
    cycleList lst =
      let n = length lst in
      [ take n $ drop i $ cycle lst
      | i <- [0.. n-1] ]