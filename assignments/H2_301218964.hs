-- CMPT 383 - Spring 2026 - Homework 2
-- Sheila Nicholson - 301218964

-- Q1: implement foldl as a recursive function
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl fn acc [] = acc
myFoldl fn acc (x:xs) = myFoldl fn (fn acc x) xs    -- recursively apply fn on tail using updated accumator resolved by applying function to current element

-- Q2: implement foldr as a recursive function
myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr fn acc [] = acc
myFoldr fn acc (x:xs) = fn x (myFoldr fn acc xs)    -- apply the fn on current element then recursively update accumulator with tail

-- Q3: write a map function that alternates the mapping between two different functions
alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b] 
alternativeMap fn1 fn2 [] = []
alternativeMap fn1 _ [x] = [fn1 x] 
alternativeMap fn1 fn2 (x:xs:xss) = fn1 x : fn2 xs : alternativeMap fn1 fn2 xss

-- Q4 create a fn that uses foldr to calculate the length of a list
myLength :: Num b => [a] -> b
myLength lst = foldr (\_ n -> n + 1) 0 lst  -- we are using the lambda to be our counting function

-- Q5: implement a filter function that uses foldl
myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter fn [] = []
myFilter fn lst = foldl decide [] lst     -- step is where we decide to add or not add the element
    where 
        decide acc x
            | fn x == True = acc ++ [x]     -- if element should be added append to accumulator (final list)
            | fn x == False = acc
-- how to think of how this expands out
-- foldl decide [] [x1, x2, x3]
-- = decide (decide (decide [] x1) x2) x3

-- Q6: takes a list and returns the sum of squares of all even numbers in the list
-- original implemenation:
-- sumsqeven :: Int a => [a] -> a
-- sumsqeven lst = sum (map (\x -> x*x) (filter even lst))

-- Q6: takes a list and returns the sum of squares of all even numbers in the list
-- point free style implementation: 
sumsqeven :: Integral a => [a] -> a
sumsqeven = sum.map(^2).filter even 