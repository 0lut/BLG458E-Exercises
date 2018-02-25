# Homework 1 Report

Sahin Olut -- 150140124

## Question 2

### a.

sundays' is calculating the n, number of sundays in 1st day of month.

### b.
Instead we would write as this
```haskell
sundays' :: Integer -> Integer -> Integer -> Integer
            sundays' y m
                | y > end = 0
                | dayOfWeek y m 1 == 1 = (+) sundays' nextY nextM  1
                | otherwise  = sundays' nextY nextM
                where
                    nextY = if m == 12 then y+1 else y
                    nextM = if m == 12 then 1 else m+1
```
It is way more expressive and Haskellish.


## Question 5

There are 146097($366*97 + 365*103$) days in 400 years. It can be divided by 7 so there are 20871 weeks in 400 years. Because of that, the day for date is same 400 years after. The date after 400 years is dependent only on which date we started, so the probability of date is sunday is $1/7$. If we know starting date already then we can calculate probability.
