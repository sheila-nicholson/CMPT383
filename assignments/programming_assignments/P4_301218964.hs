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
                    Just y -> return(y, Set.empty)                  -- if the key exists in the map return the value
                    Nothing -> return(TError, Set.singleton CError) -- if it does not exist you must return ConstrainSet CError
infer g (Abs x e) = do 
    y <- getFreshTVar
    (t, c) <- infer (Map.insert x y g) e    -- extend environment g with x:y
    return (TArr y t, c)    -- returns function type y (fresh) -> t
infer g (LetIn x e1 e2) = do 
    y <- getFreshTVar   -- we are doing a sequence of monadic computations inside InferState
    (type1, constraintSet1) <- infer g e1
    (type2, constraintSet2) <- infer (Map.insert x y g) e2
    return (type2, Set.insert (CEq y type1) (Set.union constraintSet1 constraintSet2) )     -- returns type2 (as defined in CT-Let)
                                                                                            -- constraint set = {C1 U C2 U {y=type1}}
-- infer g (App Expr Expr)




main :: IO ()
main = do
    let result = runState getFreshTVar 1    -- initialize the global counter to 1
    print result
