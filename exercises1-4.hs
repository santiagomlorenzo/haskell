-- Chapter one

-- 3

product :: Num a => [a] -> a
product [] = 1
product (firstNumber:tailNumbers) = firstNumber *  Main.product tailNumbers

-- 4

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (currentElement:elements) = qsort larger ++ [currentElement] ++ qsort smaller
  where
    smaller = [a | a <- elements, a <= currentElement]
    larger = [b | b <- elements, b > currentElement]

-- 5
-- what would be the effect of replacing <= with < ? 
-- my guess: repeated elements will dissappear
-- correct

qsortWithoutEquals :: Ord a => [a] -> [a]
qsortWithoutEquals [] = []
qsortWithoutEquals (currentElement:elements) = qsortWithoutEquals larger ++ [currentElement] ++ qsortWithoutEquals smaller
  where
    smaller = [a | a <- elements, a < currentElement]
    larger = [b | b <- elements, b > currentElement]

--------------------------------------------------------------------------------------------------------------------------

-- Chapter two

-- 2

  --2^3*4
  --(2^3)*4

  --2*3+4*5
  --((2*3)+4)*5 --No! -> (2*3)+(4*5)

  -- 2+3*4^5 --> 2+(3*(4^5)) dont know why

-- 3

n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- 4
myLast = head.reverse
myLast2 [a] = a
myLast2 (x:xs) = myLast2 xs

--5
myInit list = take amount list
  where
    amount = (-) (length list) 1
myInit2 [a] = []
myInit2 (x:xs) = [x] ++ myInit2 xs 

--------------------------------------------------------------------------------------------------------------------------

-- Chapter three

-- 1

-- ['a', 'b', 'c'] :: [Char]
-- ('a', 'b', 'c') :: (Char, Char, Char)
-- [(False, 'O'), (True, '1')] :: [(Bool, Char)]
-- ([False, True], ['0','1']) :: ([Bool], [Char])
-- [tail, init, reverse] :: [[a] -> [a]]

-- 2
bools :: [Bool]
nums :: [[Int]]
add :: Int -> Int -> Int -> Int
copy :: a -> (a,a)
apply :: (a -> b) -> a -> b

bools = [False, True]
nums = [[1,2], [3,4]]
add a b c = a + b + c
copy a = (a, a)
apply f a = f a

-- 3
second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f (f x)

second :: [a] -> a
swap :: (a,b) -> (b,a)
pair :: a -> b -> (a,b)
double :: Num a => a -> a
palindrome :: Eq a => [a] -> Bool -- forgot the EQ at first
twice :: (a -> a) -> a -> a



--------------------------------------------------------------------------------------------------------------------------

-- Chapter four

-- 1
halve :: [a] -> ([a], [a])
halve list = (take half list, drop half list)
  where
    half = length list `div` 2


-- 2
third :: [a] -> a
third = head.tail.tail
third2 (_:_:z:_) = z
third3 list = list!!2

-- 3
safetail :: [a] -> [a]
safetail (x:xs) = if null xs then [] else xs

safetail2 (x:xs) 
  | null xs = []
  | otherwise = xs
  
safetail3 [a] = []
safetail3 (x:xs) = xs

-- 8 doesnt work dont understand a phrase in the problem statement
luhnDouble :: Ord a => Num a => a -> a
luhnDouble number 
  | doubledNumber > 9 = doubledNumber - 9
  | otherwise = doubledNumber
  where
    doubledNumber = double number

luhn :: Int -> Int -> Int -> Int -> Bool
luhn first second third fourth = luhnSum `mod` 10 == 0
  where
    luhnSum = first + luhnDouble second + third + luhnDouble fourth