-- CMPT 383 - Spring 2026 - Homework 4
-- Sheila Nicholson - 301218964

-- Q1: make ErrJst e a functor

-- recall functor type class:
-- functor must have kind * -> *
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

data ErrJst e j = Err e | Jst j deriving (Show)

instance Functor (ErrJst e) where   -- note the partial decomposition of ErrJst
    fmap :: (a -> b) -> ErrJst e a -> ErrJst e b
    fmap f (Jst x) = Jst (f x)
    fmap _ (Err x) = Err x

-- Q2: make ErrJst e an applicative functor

-- recall applicative type class:
-- applicative must have kind * -> *
-- class (Functor f) => Applicative f where
--     pure :: a -> f a    -- "pure function" turn an element into an applicative functor value
--     (<*>) :: f (a -> b) -> f a -> f b  -- "ap function" similiar to fmap but it takes a function inside the applicative functor

instance Applicative (ErrJst e) where
    pure :: a -> ErrJst e a
    (<*>) :: ErrJst e (a -> b) -> ErrJst e a -> ErrJst e b
    pure a = Jst a              -- pure takes a value returns a applicative
    (Err e) <*> _ = Err e       -- Err e will always return Err e
    (Jst  e) <*> f = fmap e f   -- fmap takes in a fn and a functor : f -> f a -> f b

-- Q3: make ErrJst e an a monad

-- recall monad type class:
-- instances must have type constructors of kind * -> *
-- instances can use fmap, pure and <*> bc of inheritance
-- class Applicative m => Monad m where
--     return :: a -> m a  -- turn a normal value into a monadic value
--     (>>=) :: m a -> (a -> m b) -> m b -- "bind fn" takes the value from a monadic value, applies a fn to it and returns the result as a monadic value
--     return = pure

instance Monad (ErrJst e) where
    return :: a -> ErrJst e a
    (>>=) :: ErrJst e a -> (a -> ErrJst e b) -> ErrJst e b
    return = pure           
    (Err e) >>= _ = Err e   -- Err e always returns Err e
    (Jst e) >>= f = f e     -- apply fn to value


-- Q4: Write a function called join that can join “a monad of monadic values” into a monadic value

join :: Monad m => m (m a) -> m a     -- a monad containing monadic values returns a monadic value of same type
join mm = mm >>= \ma -> ma     
-- binding the outer monad so you can access the value (in our case is a monad) inside of it
-- think of it as >>=) :: m x -> (x -> m y) -> m y 
-- here mm :: m (m a) so this is the first param m (x), where x = m a
-- \ma -> ma :: (x -> m a) where we can think of a = y, then we retun m a (think of as m y)

-- Q5: Make LTree an instance of Foldable
-- class Monoid m where
--   mempty  :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m

-- values are only stored in the leaves
data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where
    foldMap f (LNode (lst) (rst)) = mappend (foldMap f lst) (foldMap f rst)     -- recurse the tree in lst, rst order
    foldMap f (Leaf a) = f a    -- only calc. value once you hit a leaf