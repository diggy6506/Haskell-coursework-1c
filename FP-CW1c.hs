-- Question 1 --

-- 1a

second1 :: [a] -> a
second1 xs = head(tail xs)

-- 1b

second2 :: [a] -> a
second2 xs = xs !! 1

-- 1c

second3 :: [a] -> a
second3 (_:a:_) = a

-- Question 2 --

-- 2a

xor1 :: Bool -> Bool -> Bool
xor1 True  True  = False
xor1 True  False = True
xor1 False True  = True
xor1 False False = False

-- 2b

xor2 :: Bool -> Bool -> Bool
xor2 a b = if a /= b then True else False

-- 2c

xor3 :: Bool -> Bool -> Bool
xor3 a b = a /= b

-- Question 3 --

sumsqr :: Int -> Int
sumsqr n = sum [x*x | x <- [1..n]] 

-- Question 4 --

grid :: Int -> [(Int, Int)]
grid n = [(x, y) | x <- [0..n], y <- [0..n], x /= y]

-- Question 5 --

euclid :: Int -> Int -> Int 
euclid a b = euclid' (abs a) (abs b) 
    where 
        euclid' a b 
            | a == 0 = b 
            | b == 0 = a 
            | a == b = a 
            | otherwise = euclid (max a b - min a b) (min a b)
            
-- Question 6 --

fastrev :: [a] -> [a]
fastrev xs = rev xs []

rev :: [a] -> [a] -> [a] 
rev []     ys = ys     
rev (x:xs) ys = rev xs (x:ys) 
