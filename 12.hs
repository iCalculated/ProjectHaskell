triangle n = sum [1..n] 
rmdups ls = [d|(z,d)<- zip [0..] ls,notElem d $ take z ls]

primes = 2 : primes'
    where isPrime (p:ps) n = p * p > n || n `rem` p /= 0 && isPrime ps n
          primes' = 3 : filter (isPrime primes') [5,7 ..]

prime_factors n = 
    case factors of
        [] -> [n]
        _  -> factors ++ prime_factors (div n (head factors))
        where factors = take 1 $ filter (\x -> (n `rem` x) == 0) (takeWhile (\x -> ( x * x)<n) primes)

get_freq ((p,f)) = f 
    
freq ls = [ (d,sum [1|x<-ls,d == x]) | d<-rmdups ls]

count_divisors n = product (map (\x -> x + 1) (map get_freq (freq $ prime_factors (triangle n))))
main = print $ triangle $ (length $ takeWhile (<500) $ map count_divisors [1..]) + 1