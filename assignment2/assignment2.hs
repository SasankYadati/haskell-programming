module Assignment2 where

import Data.List ( foldl' )
import Data.Word ( Word8 )
import Data.Int ( Int8 )
import Data.Ratio ( (%), denominator, numerator )


-- Problem 1
-- Check if input list is an AP. Length <= 2 means it is a trivial AP. 
isAP :: [Integer] -> Bool
isAP xs
    | length xs <= 2 = True
    | otherwise = isAP_ xs d where
        d = head (tail xs) -  head xs

isAP_ :: [Integer] -> Integer -> Bool
isAP_ [] _ = True
isAP_ [_] _ = True
isAP_ (x:(y:ys)) d = y - x == d && isAP_ ys d

-- Problem 2
-- targetSum t l checks if two elements at distinct positions of l sum to t
targetSum :: Integer -> [Integer] -> Bool
targetSum _ [] = False
targetSum _ [_] = False 
targetSum t (x:y:xs)
        | x + y == t = True
        | x + y > t = targetSum t (x:take (n-1) xs)
        | otherwise = targetSum t (x:xs) || targetSum t (y:xs)
        where
            n = length xs

-- Problem 3
-- increment a list of Word8-s representing an integer 
incr :: [Word8] -> [Word8]
incr xs = reverse (incr_ (reverse xs) True)

incr_ :: [Word8] -> Bool -> [Word8]
incr_ xs False = xs
incr_ [] True = [1]
incr_ (x:xs) True
    | x == 255 = 0 : incr_ xs True
    | otherwise = x+1:xs


-- Problem 4
-- Find Int8 value represented by list of 8 bits (in twos complement form), and find twos complement representation (consisting of 8 bits) of an Int8. 
twosVal :: [Bool] -> Int8
twosVal (False:bs) = twosValRev (reverse bs )
twosVal (True:bs) = -128 + twosValRev (reverse bs)

twosValRev :: [Bool] -> Int8
twosValRev [True] = 1
twosValRev [False] = 0
twosValRev (True:bs) = 1 + 2 * twosValRev bs
twosValRev (False:bs) = 2 * twosValRev bs


twosRep :: Int8 -> [Bool]
twosRep x 
    | x < 0 = True : twosRepRev (x+127+1) []
    | otherwise = False : twosRepRev x []

twosRepRev :: Int8 -> [Bool] -> [Bool]
twosRepRev x bs
    | length bs == 7 = bs
    | even x = twosRepRev (div x 2) (False:bs)
    | otherwise = twosRepRev (div x 2) (True:bs)

-- Problem 5
-- Compute a finite continued fraction representing a rational, and the rational represented by a finite continued fraction. 
invert :: Rational -> Rational
invert x = denominator x % numerator x
cf :: Rational -> [Integer]
cf x
    -- | p == 1 = [denominator x]
    | p `mod` q /= 0 = (fromIntegral p `div` q) : (cf $ invert ((p `mod` q) % q))
    | otherwise = [(fromIntegral p `div` q)]
    where 
        p = numerator x
        q = denominator x
computeRat :: [Integer] -> Rational
computeRat [x] = fromIntegral x
computeRat (x:xs) = fromIntegral x + invert y where
    y = computeRat xs

-- Problem 6
-- Compute approximations of sqrt 15, based on infinite continued fraction. 
computeFrac :: Rational -> Double
computeFrac x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxTarget :: Double -> Rational
approxTarget eps = 3 + approxTargetHelper eps 1

approxTargetHelper :: Double -> Rational -> Rational
approxTargetHelper eps estimate
    | abs (-3 + sqrt 15 - computeFrac estimate) < eps = estimate
    | otherwise = approxTargetHelper eps (1 / (1 + 1 / (6 + estimate)))