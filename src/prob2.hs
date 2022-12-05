-- add this flag -i../libs when compiling prob2.hs
import Cp
import Exp
import Cp2223data

-- para efeitos de teste.
test = take 10 acm_ccs

-- Esta função recebe uma certa string e devolve um par com a informação do nível da
-- árvore para onde vai a string e a respectiva string sem os espaços no início.
getLevel = (((flip div 4).length) >< id).(span (==' '))


-- gene da função tax
gene :: [String] -> Either String (String,[[String]]) -- gene :: S* -> S + S* X (S*)*
gene [] = i1 ""
gene [s] = i1 s
gene l = undefined

-- função da pergunta 1 deste problema
tax :: [String] -> Exp String String
tax = anaExp (gene)