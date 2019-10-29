module SVG.Geometry(module SVG.Geometry) where

import Prelude
import Data.Array(filter)
import Data.Maybe(Maybe(..),maybe)
import Data.Foldable(foldr)
import Data.Sparse.Polynomial(Polynomial,(^),(?))
--import Graphics.Drawing ( Drawing, closed, fillColor, filled
--                        , lineWidth, outlineColor, outlined
--                        , path, text)
--import Graphics.Drawing (circle, arc) as Drawing
--import Graphics.Drawing.Font (fantasy, font)
--import Color (Color)
import Math (atan2, cos, pi, sin, sqrt)

type PointAttributes = 
  { name :: String
  , coordinates :: Polynomial Number}

newtype Point = Point PointAttributes

point :: String -> Number -> Number -> Point
point name x y = Point $
  { name
  , coordinates: x^0+y^1}

instance showPoint :: Show Point where
  show q@(Point p) = 
    p.name <> "(" <> show (abs q) <> "," <> show (ord q) <> ")"
  
class Based a where
  abs :: a -> Number
  ord :: a -> Number
  coords :: a -> Polynomial Number
  
instance basedPoint :: Based Point where
  abs (Point p) = p.coordinates ? 0
  ord (Point p) = p.coordinates ? 1
  coords (Point p) = p.coordinates

rename :: String -> Point -> Point
rename str (Point p) = Point $ p{name=str}

newtype Segment = 
  Segment { origin :: Point
          , extremity :: Point
          , asOriented :: Maybe String}

segment :: Point -> Point -> Maybe String -> Segment
segment origin extremity asOriented = 
  Segment {origin, extremity, asOriented}

middle :: String -> Segment -> Point
middle name (Segment { origin: Point p1
                     , extremity: Point p2
                     , asOriented: _}) = 
  Point { name
        , coordinates: ( p1.coordinates 
                       + p2.coordinates) * 0.5^0}

newtype Vector = Vector (Polynomial Number)

vector :: Point -> Point -> Vector
vector (Point a) (Point b) = 
  Vector $ b.coordinates - a.coordinates
  
scale :: Number -> Vector -> Vector
scale k v@(Vector coordinates) = 
  Vector $ k^0 * coordinates

instance basedVector :: Based Vector where
  abs (Vector v) = v ? 0
  ord (Vector v) = v ? 1
  coords (Vector v) = v

class Measurable a where
  length :: a -> Number

instance measurableVector :: Measurable Vector where
  length v = sqrt $ (abs v) * (abs v) + (ord v) * (ord v)
  
instance measurableSegment :: Measurable Segment where
  length (Segment {origin, extremity, asOriented}) = 
    length $ vector origin extremity

normalized :: Vector -> Vector
normalized v = scale (1.0 / length v) v

normalTo :: Vector -> Vector
normalTo v = Vector $ (- ord v)^0+(abs v)^1

rotated :: Number -> Vector -> Vector
rotated ang v = 
  Vector $ (abs v * cos ang - ord v * sin ang)^0 +
           (abs v * sin ang + ord v * cos ang)^1

projection :: Vector -> Vector -> Vector
projection direction v = 
  scale ((abs v * abs direction + ord v * ord direction) / 
        (length direction * length direction)) direction
  
class Summable a b where
  plus :: a -> b -> a

infixl 6 plus as <+|
  
instance summableVectorVector :: Summable Vector Vector where
  plus (Vector u) (Vector v) = Vector $ u+v
  
instance summublePointVector :: Summable Point Vector where 
  plus p v = Point {name:"", coordinates: coords p + coords v}

cosAngle :: Vector -> Vector -> Number
cosAngle u v = 
         (abs u * abs v + ord u * ord v) /
         (length u * length v)

newtype HalfLine = HalfLine {origin :: Point, direction :: Vector}

halfline :: Point -> Vector -> HalfLine
halfline origin direction = HalfLine {origin, direction}

newtype Line = Line {a :: Number, b :: Number, c :: Number}

line :: Point -> Point -> Line
line m n = Line $
  { a: ord m - ord n
  , b: abs n - abs m
  , c: (abs m) * (ord n) - (ord m) * (abs n)}

aPointOnLine :: Line -> Point
aPointOnLine (Line {a,b,c}) = 
  point "" (-a*c/(a*a+b*b)) (-b*c/(a*a+b*b))

aVectorOfLine :: Line -> Vector
aVectorOfLine (Line {a,b,c}) = 
  Vector $ (-b)^0 + a^1

newtype Circle = Circle {center :: Point, radius :: Number}

circle :: Point -> Number -> Circle
circle p r = Circle {center: p, radius: r}

newtype Arc = 
  Arc { origin :: Vector
      , center :: Point
      , extremity :: Vector
      , radius :: Number
      , flipped :: Boolean
      , swapped :: Boolean
      , asOriented :: Maybe String}

arc :: Vector -> Point -> Vector -> Number 
    -> Boolean -> Boolean -> Maybe String 
    -> Arc
arc origin center extremity radius flipped swapped asOriented = 
  Arc { origin
      , center
      , extremity
      , radius
      , flipped
      , swapped
      , asOriented}

newtype RightAngle = 
  RightAngle { origin :: Vector
             , center :: Point
             , extremity :: Vector
             , radius :: Number}

rightangle :: Vector -> Point -> Vector -> Number -> RightAngle
rightangle origin center extremity radius = 
  RightAngle {origin, center,extremity, radius}

class Intersectable a b where
  meets :: a -> b -> Array Point

instance interLineLine :: Intersectable Line Line where
  meets (Line {a,b,c}) (Line {a:a',b:b',c:c'}) = 
    let delta = a*b' - a'*b
    in case unit of
     unit | delta == 0.0 -> []
          | otherwise    -> 
              [point "" ((b*c'-b'*c)/delta) ((a'*c-a*c')/delta)]

instance interLineHalfLine :: Intersectable Line HalfLine where
  meets l (HalfLine {origin, direction}) = 
    let l' = line origin (origin <+| direction)
     in filter (\p -> cosAngle (vector origin p) direction >= 0.0) $
         l `meets` l'

instance interHalfLineLine :: Intersectable HalfLine Line where
  meets hl l = meets l hl

instance interLineCircle :: Intersectable Line Circle where
  meets l@(Line {a,b,c}) (Circle {center, radius}) = 
    let m = aPointOnLine l
        u = aVectorOfLine l
        n = m <+| projection u (vector m center)
        ob = length $ vector center n
      in case unit of
        unit | ob > radius  -> []
             | ob == radius -> [n]
             | otherwise    ->
                let om = sqrt $ radius * radius - ob *ob
                    v = scale (om / length u) u
                 in [ n <+| v, n <+| (scale (-1.0) v)]

instance interCircleLine :: Intersectable Circle Line where
  meets c l = meets l c
  
instance interHalfLineCircle :: Intersectable HalfLine Circle where
  meets (HalfLine {origin, direction}) c = 
    let l' = line origin (origin <+| direction)
     in filter (\p -> cosAngle (vector origin p) direction >= 0.0) $ 
          c `meets` l'

instance interCircleHalfLine :: Intersectable Circle HalfLine where
  meets c hl = meets hl c
  
instance interCircleCircle :: Intersectable Circle Circle where
  meets (Circle {center:c0, radius: r0})
        c@(Circle {center:c1, radius: r1}) =
          let x0 = abs c0
              y0 = ord c0
              x1 = abs c1
              y1 = ord c1
              l = Line { a: 2.0 * (x0-x1)
                       , b: 2.0 * (y0-y1)
                       , c: x1 * x1 - x0 * x0 
                          + y1 * y1 - y0 * y0
                          + r0 * r0 - r1 *r1}
        in c `meets` l 

instance interHalfLineHalfLine :: Intersectable HalfLine HalfLine where
  meets (HalfLine {origin, direction}) hl = 
    let l = line origin (origin <+| direction)
     in filter (\p -> cosAngle (vector origin p) direction >= 0.0) $
         hl `meets` l

instance interSegmentLine :: Intersectable Segment Line where
  meets (Segment {origin, extremity, asOriented}) l = 
    let hl = halfline origin (vector origin extremity)
    in filter (\p -> 
        cosAngle (vector extremity p) 
                   (vector extremity origin) >= 0.0) $ hl `meets` l

instance interLineSegment :: Intersectable Line Segment where
  meets l s = meets s l

instance interSegmentHalfLine :: Intersectable Segment HalfLine where
  meets s (HalfLine {origin, direction}) = 
    let l = line origin (origin <+| direction)
    in filter (\p -> 
        cosAngle (vector origin p) 
                   direction >= 0.0) $ s `meets` l

instance interHalfLineSegment :: Intersectable HalfLine Segment where
  meets hl s = meets s hl

instance interSegmentCircle :: Intersectable Segment Circle where
  meets (Segment {origin, extremity, asOriented}) c = 
    let hl = halfline origin (vector origin extremity)
    in filter (\p -> 
        cosAngle (vector extremity p) 
                   (vector extremity origin) >= 0.0) $ hl `meets` c

instance interCircleSegment :: Intersectable Circle Segment where
  meets c s = meets s c
  
instance interSegmentSegment :: Intersectable Segment Segment where
  meets (Segment {origin, extremity, asOriented}) s = 
    let hl = halfline origin (vector origin extremity)
    in filter (\p -> 
        cosAngle (vector extremity p) 
                   (vector extremity origin) >= 0.0) $ hl `meets` s


