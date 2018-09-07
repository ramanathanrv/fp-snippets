import Prelude
import Data.Char
import Control.Monad

newtype Parser a = Parser (String -> [(a, String)]) 

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]


failure :: Parser a
failure = Parser (\cs -> [])

return' :: a -> Parser a
return' v = Parser (\cs -> [(v, cs)])

-- takes the function out and executes it
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p


-- simple combinators 

instance Monad Parser where
  (Parser p) >>= f    = Parser (\inp -> concat [parse (f v) out | (v,out) <- p inp])

instance Functor Parser where
  -- fmap (a -> b) -> Parser -> Parser
  fmap f (Parser p)   = Parser (\inp -> [(f v, out) | (v, out) <- p inp])

instance Applicative Parser where
  pure  = return
  (<*>) = ap

-- take a predicate and run it against the return value 
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do {x <- item; if p x then return' x else failure}

char  :: Char -> Parser Char
char x = satisfy (\y -> x == y)

digit :: Parser Char
digit  = satisfy isDigit

(+++)  :: Parser a -> Parser a -> Parser a
(Parser p) +++ (Parser q) = Parser (\cs -> 
            case p cs of
              []         -> q cs
              [(v, out)] -> [(v, out)]
          )

-- lets write expression parser
-- 1+2+3+4
-- expr = digit + expr | digit

dp :: Parser Int
dp  = do d <- digit
         char '+'
         e <- digit
         return' ( (digitToInt d) + (digitToInt e))

-- expr :: Parser Int
-- expr  = expr1 +++ return' 0

expr :: Parser Int
expr = do
          d1    <- digit
          let x1 = digitToInt d1
          d2     <- (do {char '+'; expr}) +++ return' 0
          return' (x1 + d2)

many  :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1  :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many p
             return' (v:vs)

expr_ext1 :: Parser Int
expr_ext1 = do
              d  <- digit
              let x = (digitToInt d)
              ds <- many (do char '+'
                             e <- digit
                             let y = (digitToInt e)
                             return' y
                          )
              return' (sum (x:ds))

expr_ext2 :: Parser Int
expr_ext2  = do
            nums <- many (digit)
            let num = read nums :: Int
            ds <- many (do char '+'
                           rhs <- many (digit)
                           let rhsi = read rhs :: Int
                           return' rhsi 
                        )
            return' (sum (num:ds))


-- Parses many digits "12+13"
expr_2 :: Parser Int
expr_2 = do
          d1       <- many (digit)
          let num1 = read d1 :: Int
          num2     <- (do {char '+'; expr_2}) +++ return' 0
          return' (num1 + num2)
