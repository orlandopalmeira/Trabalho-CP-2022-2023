-- add this flag: '-i../libs' when compiling prob1.hs
import Cp 

-- Original version
f a b c 0 = 0
f a b c 1 = 1
f a b c 2 = 1
f a b c n  = a * (f a b c (n-1)) + b * (f a b c (n-2)) + c * (f a b c (n-3))

-- Efficient version
fbl = undefined