import System.Environment
-- import Control.Applicative (Alternative)     -- only brings Alternative class into scope
import Control.Applicative (Alternative(..))    -- brings Alternative class and it's methods
import Data.Char
import System.IO

data Prop = Const Bool
    | Var String
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Iff Prop Prop
    deriving (Eq, Read, Show)

-- 3. Reuse the code that we have learned about Parser, including the Parser definition, the parse function,
-- instances of Functor, Applicative, Monad, Alternative (imported from Control.Applicative),
-- basic parsing primitives, and so on.

newtype Parser a = P (String -> [(a, String)])  -- where P is a dummy data constructor

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

-- basic parking primatives:
item :: Parser Char
item = P (\input -> case input of       -- \input arbitraty input as str
                        []      -> []   -- failure
                        (x:xs)  -> [(x, xs)])

-- Parser as functor
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b 
    fmap func par = P (\input -> case parse par input of
                                []              -> []
                                [(val, out)]    -> [(func val, out)])

-- Parser as an Applicative
instance Applicative Parser where
    pure :: a -> Parser a
    pure val = P (\input -> [(val, input)])  -- put val in paser, do not consume any input
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    parsFunc <*> parsVal = P (\input -> case parse parsFunc input of    -- take a fn out of an applicative, apply fn to applicative value 
                                        []          -> []
                                        [(func, out)] -> parse (fmap func parsVal) out)

-- Parser as a Monad -- useful for a sequence of parsing
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pars >>= func = P (\input -> case parse pars input of
                                    []          -> []
                                    [(val, out)] -> parse (func val) out) -- parse output with parser (func val)

-- Parser as an Alternative - basically gives an or fn to parser
instance Alternative Parser where
    empty :: Parser a
    empty = P (\input -> [])
    (<|>) :: Parser a -> Parser a -> Parser a
    firstParse <|> secondParse = P (\input -> case parse firstParse input of
                                                []      -> parse secondParse input
                                                [(val, out)] -> [(val, out)])

-- checks for x==condition, ex:
-- sat (=="\/")
sat :: (Char -> Bool) -> Parser Char
sat predicate = do 
            x <- item
            if predicate x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x:xs)   = do   
                    char x
                    string xs
                    return (x:xs) -- puts x:xs into parser

ident :: Parser String      -- identifiers start with a lowercase followed by alphanumeric charcters
ident = do
            x <- lower
            xs <- many alphanum
            return (x:xs)

nat :: Parser Int
nat = do
        xs <- some digit
        return (read xs)

space :: Parser ()  -- () 'unit' aka void
space = do 
        many (sat isSpace)
        return ()

token :: Parser a -> Parser a
token parse = do
                space       -- remove leading whitespace
                val <- parse
                space       -- remove trailing whitespace
                return val  -- put val in a parser container

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]        
nats = do
        symbol "["
        n <- natural
        ns <- many  (do symbol ","
                        natural)
        symbol "]"
        return (n:ns)

-- basic parsers:
-- item - consumes a single char if input is not empty
-- return v - always succeeds with the result value v - does not consume input
-- empty - always fails -> []
-- sat - returns single characters that satisfy predicate

-- from alternative:
--  many parser - applies parser 0 or more times until it fails
-- some parwer - applies parser 1 or more times 

-- 4. Write a parser constant :: Parser Prop that can parse T and F.

constant :: Parser Prop
constant = do 
            token (sat (=='T'))
            return (Const True)     -- makes Parser Prop
        <|>                         -- returns first successful parser or empty
            do 
            token (sat (=='F'))
            return (Const False)    -- make Parser Pop

-- 5. Write a parser var :: Parser Prop that can parse variables.
-- Note: variable names start with a lower-case letter followed by zero or more alphanumeric
-- characters (letters or digits)

var :: Parser Prop
var = do
        str <- ident
        return (Var str)

-- 6. Write a parser formula :: Parser Prop that can parse all possible formulas in the language of G2.

factor :: Parser Prop
factor = do
            token (sat (=='('))
            f <- formula
            token (sat (==')'))
            return f
           <|>
            constant
           <|>
            var

negation :: Parser Prop
negation = do 
                token (sat (=='!'))
                x <- negation
                return (Not x)
            <|>
                factor

andTerm :: Parser Prop
andTerm = do
            a <- negation
            symbol "/\\"
            b <- andTerm
            return (And a b)
           <|>
            negation

orTerm :: Parser Prop
orTerm = do
            a <- andTerm
            symbol "\\/"
            b <- orTerm
            return (Or a b)
          <|>
            andTerm

impTerm :: Parser Prop
impTerm = do
            a <- orTerm
            symbol "->"
            b <- impTerm
            return (Imply a b)
           <|>
            orTerm

formula :: Parser Prop
formula = do 
            a <- impTerm
            symbol "<->"
            b <- formula
            return (Iff a b)  
           <|>
            impTerm 

-- 7. Write a function parseFormula :: String -> String that takes a formula string (e.g., “x1 /\ x2”)
-- as input and generates a string as output representing the parsing result. Specifically,
-- • If the parsing succeeds and a value v of type Prop is obtained, generate the output using show v.
-- • If the parsing fails, output string “Parse Error”. Note that non-exhaustive consumption of the
-- input formula string should be considered as a parsing failure.

parseFormula :: String -> String 
parseFormula x = case parse formula x of
                    [(v,"")] -> show v
                    _        -> "Parse Error"

-- 8. Write a main to handle IO and put everything together.

main :: IO()
main = do
    file_path <- getArgs
    contents <- readFile (file_path !! 0)  -- here I am assuming there will only be 1 command line argument
    let ls = lines contents                -- each line in the file becomes an entry in a list
    let results = map parseFormula ls      -- applies checkFormulat to each line in file, adds results to list
    putStr (unlines results)               -- prints each string from list