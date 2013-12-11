primes :: [Int]
primes = sieve $ [2..]

sieve :: [Int] -> [Int]
sieve (p:s) = p:sieve [n | n <- s, n `mod` p /= 0]

reverseInt :: Int -> Int
reverseInt = read . reverse . show 

mirp :: [Int]
mirp = filter (\p -> let r = reverseInt p in r `elem` primes) primes

main = print $ take 2 mirp
