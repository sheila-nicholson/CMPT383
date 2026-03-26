-- CMPT 383 - Spring 2026 - Homework 3
-- Sheila Nicholson - 301218964

-- Q1: define a custom type (List) with data constructosr Empty and Cons.
--     create a function 'listZip' that stimulates the standard zip on two Lists.      
data List a = Cons (a) (List a) | Empty 
    deriving Show

listZip :: List a -> List b -> List (a,b)
listZip Empty _ = Empty
listZip _ Empty = Empty
listZip (Cons x xs) (Cons y ys) = Cons (x, y) (listZip xs ys)

-- Q2: Define a Tree type with data constructors EmptyTree and Node to represent a BST
--     create a function 'insert' that inserts unique elements into the BST
data Tree a = Node (a) (Tree a) (Tree a) 
    | EmptyTree
    deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert val EmptyTree = Node val EmptyTree EmptyTree
insert val (Node x lst rst)
    | val > x = Node x lst (insert val rst)
    | val < x = Node x (insert val lst) rst
    | val == x = Node x lst rst

-- Q3: Define a Nat custom type for natural numbers with data constructors Zero and Succ
--     create two functions 'natPlus' and 'natMult'
data Nat = Zero | Succ (Nat) deriving(Show)

natPlus :: Nat -> Nat -> Nat
natPlus m Zero = m
natPlus Zero n = n
natPlus (Succ m) n = natPlus m (Succ n) -- idea: move 'Succ' from m to n

-- Hint: (m + 1) · n = m · n + n
-- (m + Succ) * n = m * n + n
natMult :: Nat -> Nat -> Nat
natMult m Zero = Zero
natMult Zero n = Zero
natMult (Succ m) n = natPlus (natMult m n) n

-- Q4: Make Tree (Q2) an instance of the Eq type class without using derive(Eq)
instance (Eq a) => Eq (Tree a) where
    EmptyTree == EmptyTree = True
    Node val1 lst1 rst1 == Node val2 lst2 rst2 = (val1 == val2 && lst1 == lst2 && rst1 == rst2)
    _ == _ = False

-- Q5: make AssocList k a functor - make sure you explicitly write down the type signature of fmap for AssocList k
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

-- ghci> :k AssocList    
-- AssocList :: * -> * -> *
-- ghci> :k AssocList k
-- AssocList k :: * -> *
-- Therefore we will use AssocList k as functor

instance Functor (AssocList k) where
    fmap :: (a -> b) -> AssocList k a -> AssocList k b
    fmap _ ALEmpty = ALEmpty
    fmap f (ALCons k v lst) = ALCons k (f v) (fmap f lst)  -- must have fmap recursive call in brackets for this to work