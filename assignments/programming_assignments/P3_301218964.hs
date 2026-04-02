import qualified Data.Map.Strict as Map
import System.Environment
import System.IO

data Type = TInt
    | TBool
    | TArr Type Type
    deriving (Eq, Ord, Read, Show)

-- VarId is a type alias for string
type VarId = String

data Expr = CInt Int
    | CBool Bool
    | Var VarId
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    | ITE Expr Expr Expr
    | Abs VarId Type Expr
    | App Expr Expr
    | LetIn VarId Type Expr Expr
    deriving (Eq, Ord, Read, Show)

-- 1. Use Map from Data.Map.Strict to define the Env type for the typing environment, i.e., finish the
-- following declaration
type Env = Map.Map VarId Type

-- 2. Write an auxiliary function typingArith :: Maybe Type -> Maybe Type -> Maybe Type that
-- • returns Just TInt if both arguments are Just TInt
-- • returns Nothing otherwise
typingArith :: Maybe Type -> Maybe Type -> Maybe Type
typingArith (Just TInt) (Just TInt) = Just TInt
typingArith _ _ = Nothing

-- 3. Write an auxiliary function typingEq :: Maybe Type -> Maybe Type -> Maybe Type that
-- • returns Just TBool if both arguments are Just TInt
-- • returns Just TBool if both argumnets are Just TBool
-- • returns Nothing otherwise
typingEq :: Maybe Type -> Maybe Type -> Maybe Type
typingEq (Just TInt) (Just TInt) = Just TBool
typingEq (Just TBool) (Just TBool) = Just TBool
typingEq _ _ = Nothing

-- 4. Write a function typing :: Env -> Expr -> Maybe Type that takes a typing environment and a
-- FUN expression as input and produces as output the type of that expression based on the typing rules.
-- If there is a type error, it returns Nothing. Note that you can use the auxiliary function typingArith
-- and typingEq in this function.
typing :: Env -> Expr -> Maybe Type 
typing g (CInt _) = Just TInt
typing g (CBool _) = Just TBool
typing g (Var a) = Map.lookup a g -- Map.lookup takes a variable name and an environment 
typing g (Plus a b) = typingArith (typing g a) (typing g b)
typing g (Minus a b) = typingArith (typing g a) (typing g b)
typing g (Equal a b) = typingEq (typing g a) (typing g b)
typing g (ITE a b c) = case typing g a of   -- checking type of a -> predicate -> must be bool
                        Just TBool -> if typing g b == typing g c -- check equivalence of b and c types
                                      then typing g b -- return type of b or c
                                      else Nothing
                        _ -> Nothing -- a is not of type TBool -> type error!
typing g (Abs id ty exp) = let g' = Map.insert id ty g  -- add id + its type to new environement g'
                           in case typing g' exp of     -- use the new environment to type check the expression
                             Just expTy -> Just (TArr ty expTy) -- the type of the abstraction is: ty->expTy
                             _ -> Nothing
typing g (App exp1 exp2) = case typing g exp2 of -- get type of exp2
                                Just type1 -> case typing g exp1 of -- get type of exp1 - a function 
                                                    Just (TArr t1 t2) -> if type1 == t1  
                                                                         then Just t2
                                                                         else Nothing
                                                    _ -> Nothing
                                _ -> Nothing
typing g (LetIn var typ exp1 exp2) = let g' = Map.insert var typ g 
                                     in case typing g' exp1 of  -- notice that the type of exp1 is being checked in g' = recursive ability of 'let in'
                                        Just type1 | type1 == typ -> typing g' exp2
                                        _ -> Nothing

-- 5. Write a simple function readExpr :: String -> Expr that can read a value of Expr type from the
-- corresponding string, such as "CInt 1".
readExpr :: String -> Expr
readExpr str = read str     -- this can be done bc Expr inherits Read 

-- 6. Write a function typeCheck :: Expr -> String that takes an expression as input and produces a
-- string as output representing the type checking result. Specifically,
typeCheck :: Expr -> String
typeCheck expr = case typing Map.empty expr of      -- apply typing function to expr, return resulting type
                    Just a -> show a
                    _ -> "Type Error"

-- 7. Write a main to handle IO and put everything together
main :: IO()
main = do
    file_path <- getArgs
    contents <- readFile (file_path !! 0)       -- here I am assuming there will only be 1 command line argument
    let ls = lines contents                     -- each line in the file becomes an entry in a list
    let results = map readExpr ls               -- applies str->Expr adds results to list
    let types = map (typing Map.empty) results  -- applies partial typing function to expressions
    let output = map typeCheck results          -- applies Expr->str of type of expressions
    putStr (unlines output)                     -- prints each string from list                                                     