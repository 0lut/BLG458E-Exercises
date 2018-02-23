
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
                 
     
leap :: Integer -> Bool
leap y = (mod y 4) == 0 && (mod y 100) /= 0 || (mod y 400) == 0
