-- CMPT 383 - Spring 2026 - Homework 4
-- Sheila Nicholson - 301218964

-- Q1: make ErrJst e a functor

data ErrJst e j = Err e | Jst j deriving (Show)

instance Functor (ErrJst e) where  
    fmap :: (a -> b) -> ErrJst e a -> ErrJst e b
    fmap f (Jst x) = Jst (f x)
    fmap _ (Err x) = Err x

-- Q2: make ErrJst e an applicative functor

instance Applicative (ErrJst e) where
    pure :: a -> ErrJst e a
    (<*>) :: ErrJst e (a -> b) -> ErrJst e a -> ErrJst e b
    pure a = Jst a             
    (Err e) <*> _ = Err e      
    (Jst  e) <*> f = fmap e f   

-- Q3: make ErrJst e an a monad

instance Monad (ErrJst e) where
    return :: a -> ErrJst e a
    (>>=) :: ErrJst e a -> (a -> ErrJst e b) -> ErrJst e b
    return = pure           
    (Err e) >>= _ = Err e   
    (Jst e) >>= f = f e    

-- Q4: Write a function called join that can join 
-- a monad of monadic values into a monadic value

join :: Monad m => m (m a) -> m a     
join mm = mm >>= \ma -> ma     


-- Q5: Make LTree an instance of Foldable

data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where
    foldMap f (LNode (lst) (rst)) = mappend (foldMap f lst) (foldMap f rst)    