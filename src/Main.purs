module Main where

import Prelude
import SVG.Geometry ( arc, circle
                    , halfline, length, line, meets, middle
                    , normalTo, point, rename, rightangle
                    , segment, vector)
import SVG.Render(SVG, class Render, render')
import Data.Maybe(Maybe(..))
import Effect(Effect)
import Partial.Unsafe(unsafePartial)

data Body

foreign import body :: Effect Body
foreign import newSVG :: Body -> Effect SVG

main :: Effect Unit
main = void $ unsafePartial do
  bod <- body

  svg1 <- newSVG bod

  let transparent = "#00000000"
  let context = { svg: svg1
                , strokeWidth: 1.5
                , stroke: "#000"
                , fill: "#000"
                , fontStyle: "italic 15px arial, sans-serif"
                , textFill: "#000"}
  let render :: forall a. Render a => a -> Effect Unit 
      render = render' context
  
  let a = point "A" 310.0 320.0
  let b = point "B" 100.0 210.0
  render [a, b]
  render $ line a b
  let c = circle a (length $ vector a b)
  render' context{fill=transparent} c
  let n = normalTo $ vector a b
  let d = halfline a n
  render d
  let [e] = (rename "E") <$> (d `meets` c)
  render e
  -- Nothing means no arrow tip:
  let eb = segment e b Nothing
  render eb 
  let i = middle "I" eb
  render i
  let [f] = (rename "F") <$> c `meets` (halfline a (vector b a))
  render f
  render' context{strokeWidth = 5.0} $ segment i f $ Just "u"
  -- Two boolean parameters to adjust the arrow tip:
  let orientedArc = arc (vector i f) i (vector i b) 50.0 
                        false false false $ Just "α"
  render' context{fill=transparent} orientedArc
  let g = circle f (length $ vector i e)
  render' context{fill=transparent} g
  let [h1,h2] = g `meets` c
  render [h1,h2]
  let [h] = (rename "H") <$> (line a e) `meets` (line i f)
  render h
  render' context{fill=transparent} $ 
                        rightangle (vector a b) a (vector a e) 15.0
  -- The following line is useless but valid:
  let [] = (segment b h Nothing) `meets` (segment e f Nothing)
  let [j] = (rename "J") <$> (halfline b (vector b h)) `meets` 
                              (segment e f Nothing)
  render j
  -- render arrow tip but no vector name:
  render' context{strokeWidth = 0.5} $ segment i j $ Just ""
  pure unit

