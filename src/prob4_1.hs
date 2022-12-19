import List
import LTree
import Cp
import RelCalc
import Data.List
-- Problema 4 - versão não probabilística
type Team = String
type Group = [Team]

groups :: [Group]
groups = [["Qatar","Ecuador","Senegal","Netherlands"],
          ["England","Iran","USA","Wales"],
          ["Argentina","Saudi Arabia","Mexico","Poland"],
          ["France","Denmark","Tunisia","Australia"],
          ["Spain","Germany","Japan","Costa Rica"],
          ["Belgium","Canada","Morocco","Croatia"],
          ["Brazil","Serbia","Switzerland","Cameroon"],
          ["Portugal","Ghana","Uruguay","Korea Republic"]]

rankings = [("Argentina",4.8),("Australia",4.0),("Belgium",5.0),("Brazil",5.0),("Cameroon",4.0),("Canada",4.0),("Costa Rica",4.1),("Croatia",4.4),("Denmark",4.5),("Ecuador",4.0),("England",4.7),("France",4.8),("Germany",4.5),("Ghana",3.8),("Iran",4.2),("Japan",4.2),("Korea Republic",4.2),("Mexico",4.5),("Morocco",4.2),("Netherlands",4.6),("Poland",4.2),("Portugal",4.6),("Qatar",3.9),("Saudi Arabia",3.9),("Senegal",4.3),("Serbia",4.2),("Spain",4.7),("Switzerland",4.4),("Tunisia",4.1),("USA",4.4),("Uruguay",4.5),("Wales",4.3)]

generateMatches = pairup

arrangement = (>>=swapTeams).chunksOf 4 where
    swapTeams [[a1,a2],[b1,b2],[c1,c2],[d1,d2]] = [a1,b2,c1,d2,b1,a2,d1,c2]

rank x = 4 ** (pap rankings x-3.8)

gsCriteria = s . split (id >< id) (rank >< rank) where
    s ((s1,s2),(r1,r2)) = let d = r1 - r2 in
        if d > 0.5 then Just s1
        else if d < -0.5 then Just s2
        else Nothing

koCriteria = s. split (id >< id) (rank >< rank) where
    s ((s1,s2),(r1,r2)) = let d = r1-r2 in
        if d == 0 then s1
        else if d > 0 then s1 
             else s2

groupStage ::[Group] -> LTree Team
groupStage = initKnockoutStage.simulateGroupStage.genGroupStageMatches

type Match = (Team,Team)

genGroupStageMatches :: [Group] -> [[Match]]
genGroupStageMatches = map generateMatches

simulateGroupStage :: [[Match]] -> [[Team]]
simulateGroupStage = map (groupWinners gsCriteria)

groupWinners criteria = (best 2).consolidate.(>>=matchResult criteria)

initKnockoutStage = (anaLTree glt).arrangement

best n = (map p1).(take n).reverse.(presort p2)

consolidate ::(Num d,Eq d,Eq b) => [(b,d)] -> [(b,d)]
consolidate = (map (id >< sum)).collect

-- Trabalho a fazer
-- 1)
cgene = undefined
consolidate'::(Eq a,Num b) => [(a,b)] -> [(a,b)]
consolidate' = cataList cgene

-- 2)
matchResult :: (Match -> Maybe Team) -> Match -> [(Team,Int)]
matchResult = undefined

-- 3)
pairup :: Eq b => [b] -> [(b,b)]
pairup = undefined

-- 4)
glt = undefined