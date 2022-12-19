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

fbl a b c = p2.(for (loop a b c) initial) 
    where
        f2 _ _ _ 0 = 1
        f2 x y z n = (x * f2 x y z (n-1)) + (y * f1 x y z (n-1)) + (z * fn x y z (n-1))

        f1 x y z 0 = 1
        f1 x y z n = f2 x y z (n-1)

        fn x y z 0 = 0
        fn x y z n = f1 x y z (n-1)

        loop x y z ((f2,f1),fn) = ((x*f2 + y*f1 + z*fn, f2),f1) 
        initial = ((1,1),0) 
