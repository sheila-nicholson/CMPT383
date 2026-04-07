import qualified Data.Map.Strict as Map
import System.Environment
import System.IO
import qualified Data.Set as Set
import Control.Monad.State.Lazy

type VarId = String

data Expr = CInt Int
    | CBool Bool
    | Var VarId
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    | ITE Expr Expr Expr
    | Abs VarId Expr
    | App Expr Expr
    | LetIn VarId Expr Expr
    deriving (Eq, Ord, Read, Show)

data Type = TInt
    | TBool
    | TError
    | TVar Int
    | TArr Type Type
    deriving (Eq, Ord, Read, Show)

data Constraint = CEq Type Type
    | CError
    deriving (Eq, Ord, Read, Show)

type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]

-- 1. Define the Env type for the typing environment. Also use State from the 
-- Control.Monad.State.Lazy module to define the InferState type constructor (exact name)
-- Observe that InferState is a monad

type Env = Map.Map VarId Type       -- VarId is something like 'x' -- uses a map where VarId is the key and Type is the value
type InferState a = State Int a     -- note: InferState is a monad 

-- 2. Write a function getFreshTVar :: InferState Type returning a monadic value that yields a 
-- different type variable every time it is performed.

-- returning new type variable i.e. TVar
-- For example, the type variable X1 can be represented as TVar 1.

getFreshTVar :: InferState Type
getFreshTVar = do
    n <- get        -- gets the current state - the intial state must be set in the main function
    put (n + 1)     -- updates the global counter
    return (TVar n)  

-- 3. Write a function infer :: Env -> Expr -> InferState (Type, ConstraintSet) returning a monadic
-- value that conducts constraint-based typing when it is performed. To start with, you might use the
-- following (incomplete) code snippet

infer :: Env -> Expr -> InferState (Type, ConstraintSet)    -- this needs to be defined for all expr that use a fresh variable? perhaps not...
infer g (CInt _) = return (TInt, Set.empty)
infer g (CBool _) = return (TBool, Set.empty)
infer g (Var x) = case Map.lookup x g of
                    Just y -> return (y, Set.empty)                  -- if the key exists in the map return the value
                    Nothing -> return (TError, Set.singleton CError) -- if it does not exist you must return ConstrainSet CError
infer g (Plus e1 e2) = do
    (type1, constraint1) <- infer g e1
    (type2, constraint2) <- infer g e2
    return (TInt, Set.insert (CEq type1 TInt) (Set.insert (CEq type2 TInt) (Set.union constraint1 constraint2)))
infer g (Minus e1 e2) = do
    (type1, constraint1) <- infer g e1
    (type2, constraint2) <- infer g e2
    return (TInt, Set.insert (CEq type1 TInt) (Set.insert (CEq type2 TInt) (Set.union constraint1 constraint2)))
infer g (Equal e1 e2) = do
    (type1, constraint1) <- infer g e1
    (type2, constraint2) <- infer g e2
    return (TBool, Set.insert (CEq type1 type2) (Set.union constraint1 constraint2))    
infer g (ITE e1 e2 e3) = do 
    (type1, constraint1) <- infer g e1
    (type2, constraint2) <- infer g e2
    (type3, constraint3) <- infer g e3
    return (type2, Set.insert (CEq type1 TBool) (Set.insert (CEq type2 type3) (Set.union (Set.union constraint1 constraint2) constraint3)))
infer g (Abs x e) = do 
    y <- getFreshTVar
    (type_, constraint) <- infer (Map.insert x y g) e    -- extend environment g with x:y
    return (TArr y type_, constraint)    -- returns function type y (fresh) -> t
infer g (App e1 e2) = do
    y <- getFreshTVar
    z <- getFreshTVar
    (type1, constraint1) <- infer g e1
    (type2, constraint2) <- infer g e2
    return (z, Set.insert (CEq type1 (TArr y z)) (Set.insert (CEq type2 y) (Set.union constraint1 constraint2)))
infer g (LetIn x e1 e2) = do 
    y <- getFreshTVar   
    let g' = Map.insert x y g
    (type1, constraint1) <- infer g' e1
    (type2, constraint2) <- infer g' e2
    return (type2, Set.insert (CEq y type1) (Set.union constraint1 constraint2))     -- returns type2 (as defined in CT-Let)
                                                                                            -- constraint set = {C1 U C2 U {y=type1}}

-- 4. Write a function inferExpr :: Expr -> (Type, ConstraintSet) that takes a FUN expression as
-- input and produces as output the result of constraint-based typing. Hint: you need to “eval” the
-- monadic value defined by infer.

inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr e = evalState (infer Map.empty e) 1

-- 5. Find a function in the Set module to implement toCstrList :: ConstraintSet -> ConstraintList.
-- It can convert a constraint set to a constraint list.

toCstrList :: ConstraintSet -> ConstraintList
toCstrList a = Set.toList a 

-- 6. Recall that a substitution is a map from type variables to types. Define the type for substitutions
-- type Substitution = Map.Map Type Type
-- Write a function applySub :: Substitution -> Type -> Type that takes a substitution σ and a
-- type T as input and produces T σ as output, i.e., applying σ to T.

type Substitution = Map.Map Type Type

applySub :: Substitution -> Type -> Type
applySub sub TInt = TInt
applySub sub TBool = TBool
applySub sub (TVar num) = case Map.lookup (TVar num) sub of
                            Just typ -> typ
                            Nothing -> TVar num
applySub sub (TArr t1 t2) = TArr (applySub sub t1) (applySub sub t2)

-- 7. Write a function applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
-- that applies a substitution to a constraint list.

applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList sub ls = map (\c -> case c of
                                    CEq t1 t2 -> CEq (applySub sub t1) (applySub sub t2)
                                    CError -> CError) ls

-- 8. Write a function composeSub :: Substitution -> Substitution -> Substitution that takes two
-- substitutions σ1, σ2 as input and produces σ1 ◦ σ2 as output.

composeSub :: Substitution -> Substitution -> Substitution
composeSub sub1 sub2 = case (t11 t12) 

main :: IO ()
main = do
    -- let result = runState getFreshTVar 1    -- initialize the global counter to 1
    -- print result
    print $ inferExpr (Abs "x" (Plus (Var "x") (CInt 1)))

    -- print inference
