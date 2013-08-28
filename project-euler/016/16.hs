-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- What is the sum of the digits of the number 2^1000?
-- http://projecteuler.net/index.php?section=problems&id=16


-- stolen from http://projecteuler.net/index.php?section=forum&id=20
sumDigits :: Integer -> Integer 
sumDigits 0 = 0 
sumDigits n = (n `mod` 10) + sumDigits (n `div` 10)

powSum a = sumDigits ( 2 ^ a)

--skipped around did P20 first, this sumDigits is much better then what I had thought of.
