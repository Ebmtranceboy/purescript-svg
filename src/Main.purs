module Main where

import Prelude
import SVG.Geometry (arc, circle, halfline, length, normalTo, point, rightangle, segment, vector, (<+|))
import SVG.Render(SVG, render')
import Data.Maybe(Maybe(..))
import Effect(Effect)

data Body

foreign import body :: Effect Body
foreign import newSVG :: Body -> Effect SVG

main :: Effect Unit
main = do
        b <- body
        svg1 <- newSVG b
        let context = {svg: svg1, strokeWidth: 1.5, stroke: "#F00", fill: "#000", fontStyle: "italic 15px arial, sans-serif"}
        let pA = point "A" 10.0 10.0
        let pB = point "B" 50.0 50.0
        let h1 = halfline pA (vector pA pB)
        render' context h1  
        render' context [pA,pB]
        let pC = point "C" 20.0 40.0
        render' context $ segment pB pC $ Nothing
        let c = circle pB (length $ vector pC pB)
        render' context{fill="#00000000"} c
        let pD = pB <+| (normalTo $ vector pA pB)
        render' context pD
        render' context $ segment pB pD Nothing
        render' context{fill="#00000000"} $ rightangle (vector pB pA) pB (vector pB pD) 10.0
        render' context $ arc (vector pB pA) pB (vector pB pC) 25.0 true true Nothing
