-- add this flag -i../libs when compiling prob2.hs
import Cp
import Exp
import List
import Cp2223data

test  = take 10 acm_ccs 
test2 = take 30 acm_ccs  
-- Parte 1
out :: [String] -> Either String (String,[String])
out [s] = i1 s
out (h:t) = i2 (h,t)

gene :: [String] -> Either String (String,[[String]])
gene [] = i1 ""
gene l = ((id -|- id >< aux).out) l 
    where
        getLevel = (flip div 4).length.(takeWhile (==' ')) -- indica o nível da árvore onde uma certa string vai ficar (raíz é o nível 0)
        upLevel = map (drop 4) -- tira os primeiros 4 espaços de cada string de uma lista de strings
        aux = anaList g -- a função aux foi inspirada na função chunksOf de List.hs 
            where
                g [] = i1()
                g (h:t) = let (x,y) = span ((>1).getLevel) t in i2(upLevel (h:x), y)


tax :: [String] -> Exp String String
tax = anaExp (gene)

-- Parte 2
post :: Exp String String -> [[String]] -- incompleta
post tr = aux (tr)
    where
        aux (Var x) = [[x]]
        aux (Term x []) = [[x]]
        aux (Term x l) = map (x:) (concat(map aux l))


tudo ::[String] -> [[String]]
tudo = post.tax