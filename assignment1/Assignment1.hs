module Assignment1 where

import GHC.Num (integerFromInt)

-- problem 1
choose :: Integer -> Integer -> Integer
choose n r = factorial n `quot` (factorial (n-r) * factorial r)

factorial :: Integer -> Integer
factorial n
    | n <= 0 = 1
    | otherwise = n * factorial (n-1)


-- problem 2
primeSum :: Int -> Integer
primeSum n = sum (primes (integerFromInt n))

primes :: Integer -> [Integer]
primes n = filterPrime [2..] where
    filterPrime (p:xs) = if p <= n then p : filterPrime [x | x <- xs, x `mod` p /= 0] else []
    filterPrime [] = []


-- problem 3
leftRotate :: Integer -> Integer
leftRotate n = remaining_digits * 10 + leading_digit where
    remaining_digits = n `mod` (10 ^ (numDigits n - 1))
    leading_digit = leadingDigit n

leadingDigit :: Integer -> Integer
leadingDigit n = n `quot` (10 ^ num_digits) where
    num_digits = numDigits n - 1

numDigits :: Integer -> Integer
numDigits n
    | n < 10 = 1
    | otherwise = 1 + numDigits (n `div` 10)


-- problem 4
cLength :: Integer -> Maybe Int
cLength n
    | n == 1 = Just 0
    | even n = case cLength (n `div` 2) of
        Just x -> if x < 9999 then Just (1 + x) else Nothing
        Nothing -> Nothing
    | otherwise = case cLength (3 * n + 1) of
        Just x -> if x < 9999 then Just (1 + x) else Nothing
        Nothing -> Nothing


-- problem 5
frac :: (Int, Int, Int, Int, Int) -> Maybe (Int, Int, Int)
frac (s, x, y, z, w)
    | s /= 1 && s /= -1 = Nothing
    | x < 0 = Nothing
    | not (isSingleDigit y) || not (isSingleDigit z) || not (isSingleDigit w) = Nothing
    | s == 1 = Just (y, z, w)
    | otherwise = Just (y1, z1, w1) where
        f = (1000 - (100*y+10*z+w)) `mod` 1000
        y1 = f `div` 100
        z1 = (f `mod` 100) `div` 10
        w1 = f `mod` 10

isSingleDigit :: Int -> Bool
isSingleDigit n = n >= 0 && n < 10

-- problem 6
ilog :: Integer -> Integer -> Integer
ilog n b
    | n < b = 0
    | otherwise = 1 + ilog (n `div` b) b


-- problem 7
cos1 :: Integer -> Double -> Double
cos1 n x = s where 
    (_,_,s) = cos1helper n x


cos1helper :: (Integral a, Fractional c) => a -> c -> (a, c, c)
cos1helper n x
    | n == 1 = (1, 1.0, 1.0)
    | otherwise = (n, next_l, s + next_l)  where
        (_, l, s) = cos1helper (n-1) x
        next_l = getNextTerm n x l

cos2 :: Double -> Double -> Double
cos2 e x = cos2helper e x 1.0 1 1

cos2helper :: Double -> Double -> Double -> Integer -> Double -> Double
cos2helper e x s n c
    | abs (s - cos x) <= e = s
    | otherwise = cos2helper e x (s + nextTerm) (n+1) nextTerm where
        nextTerm = getNextTerm (n+1) x c

getNextTerm m x c = sign * abs c * (x / a) * (x / b) where
        sign = if even m then -1.0 else 1.0
        a = fromIntegral (2*(m-1))
        b = a - 1