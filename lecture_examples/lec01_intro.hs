-- to use the interative interpreter (GHCi) run ghci in terminal -> 'it' variable remembers the result of the last expression

-- to run a program -> 'ghc program.hs' to compile -> ./program.exe to execute
-- or you can load the file directly into ghci -> open ghci, ':l program.hs' 
-- if the file is edited, it can be reloaded via -> ':r program.hs'

-- to check type of variable/fn ':t var'

-- list cons ':' list prepend
-- takes an element E and a list L as input and produces as output a new list with E prepended to L
-- :t (:)
-- (:) :: a -> [a] -> [a]

-- list append '++'
-- :t (++)
-- (++) :: [a] -> [a] -> [a]
-- 'a' is a type variable - it represents various types

-- return first element of list 'head'
-- return list with first element removed 'tail'

-- :t (==)
-- (==) :: Eq a => a -> a -> Bool
-- 'Eq a' is a class constraint, fn takes two input values of the same type and returns a bool
-- the type of these two input values must be an instance of the Eq type class
-- same for /= 

-- comparision fns > < >= <= have class constraint 'Ord' - total order (any two values are comparable)

-- type classes:
-- Show - represent instances as strs
-- Read - take in string return specified type
-- ex: read "42" :: Int
-- 42
-- :t read
-- read :: Read a => String -> a
-- Num - numeric type class, its instances act like numbers (types: Int, Integer, Float, Double), fns: + -  *
-- :t 42
-- 42 :: Num a => a
-- :t (*)
-- (*) :: Num a => a -> a -> a
-- Integral - whole numbers (types: Int, Integer)
-- :t fromIntegral - turns and integral into a number
-- fromIntegral :: (Integral a, Num b) => a -> b

-- type of parameter1 -> type of parameter 2 -> type of parameter n -> return type
-- to check type of function, can use ':t  func'
half :: Float -> Float
half x = x/2

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

main = do
    putStrLn "Hello, world!"



    
   
    
