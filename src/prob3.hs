import Rose
import Svg
import Control.Concurrent (threadDelay)
-- Problema 3
type Square = (Point, Side)
type Side = Double
type Point = (Double, Double)


squares :: (Square, Int) -> Rose Square
squares = anaRose gsq


rose2List :: Rose Square -> [Square]
rose2List = cataRose gr2l


gr2l :: (Square,[[Square]]) -> [Square]
gr2l (sq,l) = sq:concat l


gsq :: (Square,Int) -> (Square,[(Square,Int)])
gsq (((x, y), l),0) = let c = l/3 in (((x+c,y+c),c),[])
gsq (((x, y), l),n) = (((x+c,y+c),c),nexts)
    where
        c = l/3
        nexts = [(((x,y),c),n-1),
                 (((x,y+c),c),n-1),
                 (((x,y+2*c),c),n-1),
                 (((x+c,y),c),n-1),
                 (((x+c,y+2*c),c),n-1),
                 (((x+2*c,y),c),n-1),
                 (((x+2*c,y+c),c),n-1),
                 (((x+2*c,y+2*c),c),n-1)]


sierpinski::(Square,Int) -> [Square]
sierpinski = hyloRose gr2l gsq


sierp4 = drawSq (sierpinski (((0,0),32),3))
constructSierp5 = do {
    drawSq (sierpinski (((0,0),32),0));
    await;
    drawSq (sierpinski (((0,0),32),1));
    await;
    drawSq (sierpinski (((0,0),32),2));
    await;
    drawSq (sierpinski (((0,0),32),3));
    await;
    drawSq (sierpinski (((0,0),32),4));
    await;
}


drawSq x = picd'' [Svg.scale 0.44 (0,0) (x>>=sq2svg)]
sq2svg (p,l) = (color "#67AB9F" . polyg) [p,p .+ (0,l),p .+ (l,l),p .+ (l,0)]
await = threadDelay 1000000 


carpets :: Int -> [[Square]]
carpets n = [sierpinski (((0,0),32),i) | i <- [0..n-1]]


present ::[[Square]] -> IO [()]
present [] = return []
present (x:xs) = do{
    drawSq x;
    await;
    present xs;
}


constructSierp :: Int -> IO [()]
constructSierp = present.carpets