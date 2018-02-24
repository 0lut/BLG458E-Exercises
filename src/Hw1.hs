module Hw1
    (dayOfWeek, sundays1, leap, isIn, daysInMonth)
    where

dayOfWeek :: (Integral a) => a -> a -> a -> a
dayOfWeek y m d = z
                where
                    z = mod (d + t1 + k + t2 + t3 + 5*j) 7
                    t3 = floor $ fromIntegral j/4
                    t2 = floor $ fromIntegral k / 4
                    t1 = floor $ fromIntegral (13 * (m' + 1)) / 5.0
                    j = div y 100
                    k = mod y 100
                    m' =  if m<=2 then m +12 else m 

sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
                where
                    sundays' :: Integer -> Integer -> Integer
                    sundays' y m
                        | y > end = 0
                        | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
                        where
                            nextY = if m == 12 then y+1 else y
                            nextM = if m == 12 then 1 else m+1
                            rest = sundays' nextY nextM
                    
                    
    
leap :: Integer -> Bool
leap y = (mod y 4) == 0 && (mod y 100) /= 0 || (mod y 400) == 0

isIn :: Ord (a) => a -> [a] -> Bool
isIn _ [] = False
isIn x xs = head xs == x || isIn x (tail xs)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth 2 y = if leap y then 29 else 28
daysInMonth m _ = if  m `isIn` [4,6,9,11] then 30 else 31


