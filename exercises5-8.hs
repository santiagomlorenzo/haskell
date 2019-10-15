-- Chapter five

-- 1
sumOfSquares = sum [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int,Int)]
grid rows columns = [(x,y) | x <- [0..rows], y <- [0..columns]]

-- 3
square :: Int -> [(Int,Int)]
square size = [(x,y) | x <- [0..size], y <- [0..size], x /= y]

-- 4
myReplicate :: Int -> a -> [a]
myReplicate repetitions element = [element | _ <- [1..repetitions]]

-- 5
pythagoreans :: Int -> [(Int,Int,Int)]
pythagoreans limit = [(x,y,z) | x <- list, 
                                y <- list, 
                                z <- list, 
                                y^2 + x^2 == z^2]
  where
    list = [1..limit]


------------------------------------------------------------------------------------------------------------------------

-- Chapter six

-- 1

factorial :: Int -> Int
factorial 0 = 1
factorial n
  | n > 0 = n * factorial (n - 1)
  | otherwise = 0


-- 2

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n 
  | n > 0 = n + sumdown (n - 1)
  | otherwise = 0

-- 3

exponentiate :: Int -> Int -> Int
exponentiate n 0 = 1
exponentiate n m 
  | n > 0 = n * exponentiate n (m - 1)
  | otherwise = 0

-- 4

euclid :: Int -> Int -> Int
euclid n m
  | n == m = n
  | n > m = euclid (n - m) m
  | otherwise = euclid n (m - n)
