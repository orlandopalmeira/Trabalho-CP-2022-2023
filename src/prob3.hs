import Rose
import Svg
import Control.Concurrent (threadDelay)
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
squares (sq@((x, y), l), n) = let c = l/3 in Rose sq $ map squares [
    (((x-2*c,y+4*c),c),n-1),
    (((x+c,y+4*c),c),n-1),
    (((x+4*c,y+4*c),c),n-1),
    (((x-2*c,y+c),c),n-1),
    (((x+4*c,y+c),c),n-1),
    (((x-2*c,y-2*c),c),n-1),
    (((x+c,y-2*c),c),n-1),
    (((x+4*c,y-2*c),c),n-1)
  ]

-- Define the 'rose2List' function which takes a rose tree and returns a list of
-- its elements in breadth-first order
rose2List :: Rose a -> [a]
rose2List (Rose x []) = [x]
rose2List (Rose x rs) = x : concatMap rose2List rs


gr2l :: (Square,[[Square]]) -> [Square]
gr2l (sq,l) = sq:concat l

gsq :: (Square,Int) -> (Square,[(Square,Int)])
gsq (sq,0) = (sq,[])
gsq (sq@((x, y), l),n) = (sq,nexts)
    where
        c = l/3
        nexts = [(((x-2*c,y+4*c),c),n-1),
                 (((x+c,y+4*c),c),n-1),
                 (((x+4*c,y+4*c),c),n-1),
                 (((x-2*c,y+c),c),n-1),
                 (((x+4*c,y+c),c),n-1),
                 (((x-2*c,y-2*c),c),n-1),
                 (((x+c,y-2*c),c),n-1),
                 (((x+4*c,y-2*c),c),n-1)]


{-

(((x-4*c,y-2*c),c),n-1),
(((x-4*c,y+c),c),n-1),
(((x-4*c,y+4*c),c),n-1),
(((x-c,y-2*c),c),n-1),
(((x-c,y+4*c),c),n-1),
(((x+2*c,y-2*c),c),n-1),
(((x+2*c,y+c),c),n-1),
(((x+2*c,y+4*c),c),n-1)

[(((x,y),l/3), n - 1),
(((x+l/3,y),l/3), n - 1),
(((x+2*l/3,y),l/3), n - 1),
(((x,y+l/3),l/3), n - 1),
(((x+2*l/3,y+l/3),l/3), n - 1),
(((x,y+2*l/3),l/3), n - 1),
(((x+l/3,y+2*l/3),l/3), n - 1),
(((x+2*l/3,y+2*l/3),l/3), n - 1)]

[(((x, y), l/3), n - 1),
(((x + l/3, y), l/3), n - 1),
(((x + 2*l/3, y), l/3), n - 1),
(((x, y + l/3), l/3), n - 1),
(((x + 2*l/3, y + l/3), l/3), n - 1),
(((x, y + 2*l/3), l/3), n - 1),
(((x + l/3, y + 2*l/3), l/3), n - 1),
(((x + 2*l/3, y + 2*l/3), l/3), n - 1)]-}

-- hyloRose :: ((b, [c]) -> c) -> (a -> (b, [a])) -> a -> c
sierpinski::(Square,Int) -> [Square]
sierpinski = rose2List.squares


sierp4 = drawSq (sierpinski (((0,0),16),3))
constructSierp5 = do {
    drawSq (sierpinski (((0,0),16),0));
    await;
    drawSq (sierpinski (((0,0),16),1));
    await;
    drawSq (sierpinski (((0,0),16),2));
    await;
    drawSq (sierpinski (((0,0),16),3));
    await;
    drawSq (sierpinski (((0,0),16),4));
    await;
    drawSq (sierpinski (((0,0),16),5));
    await;
}

drawSq x = picd'' [Svg.scale 0.44 (0,0) (x>>=sq2svg)]
sq2svg (p,l) = (color "#67AB9F" . polyg) [p,p .+ (0,l),p .+ (l,l),p .+ (l,0)]
await = threadDelay 1000000 
