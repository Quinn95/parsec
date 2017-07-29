import Text.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad





data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                | Multiply Expression Expression
                | Divide Expression Expression
                deriving (Show)

eval :: Expression -> Int
eval (Number x)       = x
eval (Add e1 e2)      = eval e1 + eval e2
eval (Subtract e1 e2) = eval e1 - eval e2
eval (Multiply e1 e2) = eval e1 * eval e2
eval (Divide e1 e2)   = eval e1 `div` eval e2

readInt :: String -> Int
readInt = read

parseNumber :: Parsec [Char] () Expression
parseNumber = fmap (Number . readInt) $ many1 digit

parseAdd :: Parsec [Char] () Expression
parseAdd = do
    string "+ "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Add e1 e2

parseSubtract :: Parsec [Char] () Expression
parseSubtract = do
    string "- "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Subtract e1 e2

parseMultiply :: Parsec [Char] () Expression
parseMultiply = do
    string "* "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Multiply e1 e2



parseDivide :: Parsec [Char] () Expression
parseDivide = do
    string "* "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Divide e1 e2


parseExpression :: Parsec [Char] () Expression
parseExpression = try parseNumber <|> try parseAdd <|> 
                  try parseSubtract <|> try parseMultiply <|> try parseDivide 


a = runParser parseExpression () "" "+ - 3 4 5"

getString :: Either a Expression -> String
getString (Left _) = "Failed"
getString (Right x) = show x
