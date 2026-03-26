-- CMPT 383 - Spring 2026 - Homework 1
-- Sheila Nicholson - 301218964

-- Q1: nth Fibonacci number
fib :: Integral a => a -> a
fib n
    | n==0       =0
    | n==1       =1
    | otherwise = fib(n-1) + fib(n-2)

-- Q2: reverse list
listReverse :: [a] -> [a]       -- input and output lists contain the same type of elements
listReverse [] = []             -- recursive base case
listReverse (h:t) = listReverse(t) ++ [h]

-- Q3: add/combine lists
listAdd :: Integral a => [a] -> [a] -> [a]
listAdd [] [] = []                              -- recursive base case
listAdd (h:t) [] = [h] ++ listAdd t []          -- case where second list empty @ element
listAdd [] (h:t) = [h] ++ listAdd [] t          -- case where first list empty @ element
listAdd (h:t) (x:y) = [(h+x)] ++ listAdd t y    -- case where first and second list elements are added

-- Q4: check if element is in list
inList :: Eq a => [a] -> a -> Bool
inList [x] y                                   -- list is singleton - recursive base case
    | x == y = True
    | x /= y = False     
inList (h:t) x                            
    | h == x = True
    | h /= x = inList t x       

-- Q5: takes a list of numbers and returns their sum via tail recursion 
sumTailRec :: Num a => [a] -> a
sumTailRec a = sumTailRecAux a 0
    where
        sumTailRecAux [] r = r                              -- r = intermediate result
        sumTailRecAux (h:t) r = sumTailRecAux t (r + h)     -- recursion on tail, sum result