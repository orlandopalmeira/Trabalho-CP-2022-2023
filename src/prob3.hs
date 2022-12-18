import Rose
-- Problema 3
-- Define a type 'Rose a' to represent a rose tree where the
-- children are of the same type 'a'

-- Define the 'Square' type as a tuple containing a point and a side
type Square = (Point, Side)
type Side = Double
type Point = (Double, Double)

-- Define the 'squares' function which takes a square and an integer depth
-- and returns a rose tree of squares
squares :: (Square, Int) -> Rose Square
squares (sq, 0) = Rose sq []
squares (sq@((x, y), l), n) = Rose sq $ map squares [
    (((x, y), l/3), n - 1),
    (((x + l/3, y), l/3), n - 1),
    (((x + 2*l/3, y), l/3), n - 1),
    (((x, y + l/3), l/3), n - 1),
    (((x + 2*l/3, y + l/3), l/3), n - 1),
    (((x, y + 2*l/3), l/3), n - 1),
    (((x + l/3, y + 2*l/3), l/3), n - 1),
    (((x + 2*l/3, y + 2*l/3), l/3), n - 1)
  ]

-- Define the 'rose2List' function which takes a rose tree and returns a list of
-- its elements in breadth-first order
rose2List :: Rose a -> [a]
rose2List (Rose x []) = [x]
rose2List (Rose x rs) = x : concatMap rose2List rs

--sierpinski = hyloRose rose2List squares 


drawSq x = picd'' [Svg.scale 0.44 (0,0) (x>>=sq2svg)]
sq2svg (p,l) = (color "#67AB9F" · polyg) [p,p .+ (0,l),p .+ (l,l),p .+ (l,0)]
await = threadDelay 1000000