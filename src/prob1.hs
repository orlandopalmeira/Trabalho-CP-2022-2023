-- add this flag: '-i../libs' when compiling prob1.hs
import Cp
import Nat

-- Original version
f a b c 0 = 0
f a b c 1 = 1
f a b c 2 = 1
f a b c n  = a * (f a b c (n-1)) + b * (f a b c (n-2)) + c * (f a b c (n-3))

-- Efficient version
{-
!!RESOLUÇÃO!! NÃO APAGUEM ISTO
Vamos definir as seguintes funções:
    (f1 a b c) n = (f a b c) (n+1)
    (f2 a b c) n = (f a b c) (n+2)
Logo,
    (f2 a b c) 0 = 1
    (f2 a b c) (n+1) = a * (f2 a b c) n + b * (f1 a b c) n + c * f a b c n

    (f1 a b c) 0 = 1
    (f1 a b c) (n+1) = (f2 a b c) n

    (f a b c) 0 = 0
    (f a b c) (n+1) = (f1 a b c) n
-}

fbl a b c = p2.(for loop initial) 
    where
        f2 0 = 1
        f2 n = (a * f2 (n-1)) + (b * f1 (n-1)) + (c * f' (n-1))

        f1 0 = 1
        f1 n = f2 (n-1)

        f' 0 = 0
        f' n = f1 (n-1)

        loop ((f2,f1),f') = ((a*f2 + b*f1 + c*(f'), f2),f1) 
        initial = ((1,1),0) 
