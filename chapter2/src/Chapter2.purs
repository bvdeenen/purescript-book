module Chapter2 where

import Math
import Debug.Trace

circleArea :: Number -> Number
circleArea r = Math.pi * ( Math.pow r 2 )
diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

main = print (circleArea 3 )
