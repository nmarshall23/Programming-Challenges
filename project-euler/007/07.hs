-- By listing the first six prime numbers: 
-- 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10001st prime number?

primes = sieve [2..] where
    sieve (x:xs) = x:(sieve [a | a <- xs, a `mod` x /= 0 ])

tenK01 = primes !! 10001
