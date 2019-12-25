f a b = sum [ e | e <- [2..(a*b-1)], gcd e (a*b) == 1, gcd (e-1) a == 2, gcd (e-1) b == 2]
main = print $ f 1008 3642