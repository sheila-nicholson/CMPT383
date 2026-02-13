-- CMPT 383 - Spring 2026 - Programming Assignment 1
-- Sheila Nicholson - 301218964

import qualified Data.Map.Strict as Map
import System.IO
import System.Environment
import Data.List

type VarId = String

data Prop = Const Bool -- data constructors
    | Var VarId     
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Iff Prop Prop
    deriving (Eq, Read, Show)

type VarAsgn = Map.Map VarId Bool   -- the key is varId and the corresponding value is a bool

imply :: Bool -> Bool -> Bool
imply True False = False
imply _ _ = True

iff :: Bool -> Bool -> Bool
iff True True = True
iff False False = True
iff _ _ = False

-- since we only want distinct variable values I will utilize:
-- nub :: Eq a => [a] -> [a]
-- nub fn removes duplicate elements from a list. It keeps the first occurrence of each element
findVarIds :: Prop -> [VarId]
findVarIds (Var x) = [x]
findVarIds (Not p) = findVarIds p   -- not necessary to use nub here as we are not appending lists for this pattern
findVarIds (And p1 p2) = nub (findVarIds p1 ++ findVarIds p2)
findVarIds (Or p1 p2) = nub (findVarIds p1 ++ findVarIds p2)
findVarIds (Imply p1 p2) = nub (findVarIds p1 ++ findVarIds p2)
findVarIds (Iff p1 p2) = nub (findVarIds p1 ++ findVarIds p2)
findVarIds _ = []       -- not sure if this is necessary....

-- idea of this: recurse down to base case, here a map in a list is created
-- as recursion bubbles back up, take all assignments for xs (elems) and extend each
-- map in two ways by inserting x=True and x=False. each step doubles the number
-- of assignments hence 2^n total variable assignments.
genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns (x:xs) = 
    let elems = genVarAsgns xs 
    in map(Map.insert x True) elems ++ map (Map.insert x False) elems
genVarAsgns [] = [Map.empty]

-- Map.! returns the value for the given key
eval :: Prop -> VarAsgn -> Bool
eval (Const c) map = c
eval (Var v) map = map Map.! v      -- returns the value for the given key (i.e. a bool)
eval (Not p) map = not (eval p map)
eval (And p1 p2) map = (&&) (eval p1 map) (eval p2 map)
eval (Or p1 p2) map = (||) (eval p1 map) (eval p2 map)
eval (Imply p1 p2) map = imply (eval p1 map) (eval p2 map)
eval (Iff p1 p2) map = iff (eval p1 map) (eval p2 map)
   
sat :: Prop -> Bool
sat e = (elem True results)     -- checks if the results list (:t [Bool]) contains True - if so formula is satisfiable
    where 
        assign = genVarAsgns(findVarIds e) -- resolves variables and creates all combinations of variable assignments
        results = map (eval e) assign      -- since functions are curried (eval e) returns a function which is then applied to assign 
                                           -- i.e. we partially apply eval
                                           -- this is necessary as the first param of map is a unary function (eval is binary fn)

-- this one really confused me, but due to the fact that the custom type Prop derived from Read we are able to utilize
-- Read's 'read' function to help generate an instance of Prop from a String
readFormula :: String -> Prop
readFormula s = read s

checkFormula :: String -> String 
checkFormula s 
    | sat (readFormula s) == True = "SAT"
    | sat (readFormula s) == False = "UNSAT"

main :: IO()
main = do
    file_path <- getArgs
    contents <- readFile (file_path !! 0)  -- here I am assuming there will only be 1 command line argument
    let ls = lines contents                -- each line in the file becomes an entry in a list
    let results = map checkFormula ls      -- applies checkFormulat to each line in file, adds results to list
    putStr (unlines results)               -- prints each string from list