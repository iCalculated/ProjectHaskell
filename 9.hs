
triples = [(a+b+c,a*b*c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2+b^2==c^2]
triple = head $ take 1 $ filter (\(a,b) -> 1000 `mod` a == 0) triples
main = print $ (1000 `div` fst triple)^3 * snd triple