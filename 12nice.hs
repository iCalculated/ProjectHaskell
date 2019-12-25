-- I was wondering how the wiki solution was so much nicer than mine
-- They imported List so I'll consider that cheating :p
primes = 2 : primes'
    where isPrime (p:ps) n = p * p > n || n `rem` p /= 0 && isPrime ps n
          primes' = 3 : filter (isPrime primes') [5,7 ..]
prime_factors n = 
    case factors of
        [] -> [n]
        _  -> factors ++ prime_factors (div n (head factors))
        where factors = take 1 $ filter (\x -> (n `rem` x) == 0) (takeWhile (\x -> ( x * x)<n) primes)

main = print $ head $ filter ((>500) . divisors) triangle_nums
    where divisors n = product $ map ((+1) . length) (group (prime_factors n))
          triangle_nums = scanl1 (+) [1..]