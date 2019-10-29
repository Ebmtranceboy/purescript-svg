module SVG.Render where

import Prelude
import Effect(Effect)
import SVG.Geometry (Arc(..), Circle(..), HalfLine(..), Line, Point(..), RightAngle(..), Segment(..), Vector(..), aPointOnLine, aVectorOfLine, abs, halfline, length, middle, ord, projection, rotated, scale, segment, vector, (<+|))
import Data.Enum(toEnum)
import Data.String(singleton)
import Data.Maybe(Maybe(..),maybe,fromJust)
import Data.Foldable(foldr)
import Partial.Unsafe(unsafePartial)
import Data.Sparse.Polynomial((^))
import Math (atan2, pi)

data SVG

type Color = String 
type FontStyle = String
type Position = Number
type Size = Number
type Path = String

foreign import svgline :: SVG 
                       -> Position -> Position 
                       -> Position -> Position 
                       -> Color -> Size -> Effect Unit
foreign import svgtext :: SVG 
                       -> Position -> Position 
                       -> Color -> FontStyle 
                       -> String -> Effect Unit
foreign import svgpath :: SVG
                       -> Color -> Size
                       -> Color 
                       -> Path -> Effect Unit

-- animate :: forall scene. ABehavior Event scene -> (scene -> Effect Unit) -> Effect (Effect Unit)

type Context = 
   { svg :: SVG
   , stroke :: Color
   , fill :: Color
   , strokeWidth :: Size
   , fontStyle :: FontStyle}

class Render a where
  render' :: Context -> a -> Effect Unit

instance renderPoint :: Render Point where
  render' {svg, stroke, strokeWidth, fill, fontStyle} 
           p@(Point {name, coordinates}) = do
    svgline svg (abs p - 5.0) (ord p - 5.0)
                (abs p + 5.0) (ord p + 5.0) 
                stroke strokeWidth
    svgline svg (abs p - 5.0) (ord p + 5.0)
                (abs p + 5.0) (ord p - 5.0)
                stroke strokeWidth
    svgtext svg (abs p + 10.0) (ord p - 10.0) 
                fill fontStyle 
                name

instance renderHalfLine :: Render HalfLine where
  render' {svg, stroke, strokeWidth} 
           (HalfLine {origin, direction}) = 
    let far = origin <+| scale 10.0 direction
    in svgline svg (abs origin) (ord origin) 
                   (abs far) (ord far) 
                   stroke strokeWidth

instance renderLine :: Render Line where
  render' ctx l = 
    let m = aPointOnLine l
        v = aVectorOfLine l
     in render' ctx [ halfline m v
                    , halfline m (scale (-1.0) v)] 

arrowBluntness = 0.3 :: Number
arrowLength = 20.0 :: Number

arrowTip :: Segment -> {at1 :: Point, at2 :: Point}
arrowTip s@(Segment {origin, extremity, asOriented}) = 
  let v = vector origin extremity
      ang = atan2 (ord v) (abs v)
      v0 = Vector $ (length v)^0
      f theta = 
        let v1 = rotated theta $ Vector $ arrowLength^0
          in origin <+| (rotated ang $ v1 <+| v0)
   in { at1: f (pi - arrowBluntness)
      , at2: f (pi + arrowBluntness)}

instance renderSegment :: Render Segment where
  render' {svg, stroke, strokeWidth, fill, fontStyle} 
           s@(Segment {origin,extremity,asOriented}) = do
    svgline svg (abs origin) (ord origin)
                (abs extremity) (ord extremity)
                stroke strokeWidth
    maybe (pure unit) (\str -> 
          let {at1, at2} = arrowTip s
          in svgpath svg stroke strokeWidth fill $
                 "M " <> (show $ abs at1) <> " " <> (show $ ord at1) <> " "
              <> "L " <> (show $ abs extremity) <> " " <> (show $ ord extremity) <> " "
              <> "L " <> (show $ abs at2) <> " " <> (show $ ord at2) <> " "
              <> "Z" ) asOriented
    let m = middle "" s
    maybe (pure unit) (\str -> do
            svgtext svg (abs m + 10.0) (ord m - 10.0) 
                        fill fontStyle
                        str
            svgtext svg (abs m + 10.0) (ord m - 23.0) 
                        fill fontStyle 
                                 (if str=="" then "" else singleton (unsafePartial $ fromJust $ toEnum 0x2192))) asOriented

instance renderCircle :: Render Circle where
  render' {svg, stroke, strokeWidth, fill} 
           (Circle{center: c,radius}) = do
    svgpath svg stroke strokeWidth fill $
            "M " <> (show $ abs c - radius) <> " " <> (show $ ord c) <> " "
         <> "a " <> (show radius) <> " " <> (show radius) <> " "
                 <> "0 1 0 " <> (show $ 2.0 * radius) <> " 0" 
    svgpath svg stroke strokeWidth fill $
            "M " <> (show $ abs c - radius) <> " " <> (show $ ord c) <> " "
         <> "a " <> (show radius) <> " " <> (show radius) <> " "
                 <> "0 1 1 " <> (show $ 2.0 * radius) <> " 0" 

instance renderArc :: Render Arc where
  render' {svg, stroke, strokeWidth, fill, fontStyle} 
         (Arc { origin, center, extremity, radius
              , flipped, swapped, asOriented}) = 
    let u = scale (radius / length origin) origin
        pO = center <+| u 
        v = scale (radius / length extremity) extremity
        pE = center <+| v
     in do
          svgpath svg stroke strokeWidth fill $
                "M " <> (show $ abs pO) <> " " <> (show $ ord pO) <> " "
             <> "a " <> show radius <> " " <> show radius <> " "
             <> "0 0 " <> (if swapped then "0" else "1") <> " "
             <> (show $ abs pE - abs pO) <> " " <> (show $ ord pE - ord pO)  
               
          let d = 0.8 - 0.004 * radius
          let p = (scale (if flipped then d else -d) v) 
                      <+| origin 
                      <+| (scale (-1.0) $ projection extremity origin)
          let n = pE <+| (scale (if flipped then -1.0 else 1.0) p)
          let {at1, at2} = arrowTip $ segment n pE Nothing
          maybe (pure unit) (\str ->
                 svgpath svg stroke strokeWidth fill $
                        "M " <> (show $ abs at2) <> " " <> (show $ ord at2) <> " "
                    <> "L " <> (show $ abs pE) <> " " <> (show $ ord pE) <> " "
                    <> "L " <> (show $ abs at1) <> " " <> (show $ ord at1)
                    <> " Z" ) asOriented
          let uv = u <+| v
          let i = center <+| scale (radius * 0.8 / length uv) uv
          maybe (pure unit) (\str -> 
                  svgtext svg (abs i) (ord i) 
                     fill fontStyle
                     str) asOriented

instance renderRightAngle :: Render RightAngle where
  render' {svg, stroke, strokeWidth, fill} 
         (RightAngle {origin, center, extremity, radius}) = 
    let v = scale (radius/length extremity) extremity
        w = scale (radius/length extremity) origin
        u = v <+| w
        m = center <+| u
        n = center <+| v
        o = center <+| w
       in svgpath svg stroke strokeWidth fill $
             "M " <> (show $ abs o) <> " " <> (show $ ord o) <> " "
          <> "L " <> (show $ abs m) <> " " <> (show $ ord m) <> " "
          <> "L " <> (show $ abs n) <> " " <> (show $ ord n)
  
instance renderSequence :: Render a => Render (Array a) where        
  render' ctx arr = foldr (<>) mempty $ (render' ctx) <$> arr

