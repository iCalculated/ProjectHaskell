primes = 2 : primes'
    where isPrime (p:ps) n = p * p > n || n `rem` p /= 0 && isPrime ps n
          primes' = 3 : filter (isPrime primes') [5,7 ..]
num = 600851475143 

prime_factors n = 
    case factors of
        [] -> [n]
        _  -> factors ++ prime_factors (div n (head factors))
        where factors = take 1 $ filter (\x -> (n `rem` x) == 0) (takeWhile (\x -> ( x * x)<n) primes)
main = print $ prime_factors(num)