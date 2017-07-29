import Text.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad


import qualified Data.HashMap.Strict as Map

-- (Name,   Value)
type Variables = Map.HashMap String Expression 


readVariable :: String -> Variables -> Expression 
readVariable name vars = case (Map.lookup name vars) of 
                           Nothing -> error "Variable not initialized"
                           (Just x') -> x'

writeVariable :: (String, Expression) -> Variables -> Variables
writeVariable (name, value) vars = case (Map.member name vars) of
                                     True -> error "Variable already initialized"
                                     _ -> Map.insert name value vars




data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                | Multiply Expression Expression
                | Divide Expression Expression
                deriving (Show)

eval :: Expression -> Int
eval (Number x)        = x
eval (Add e1 e2)       = eval e1 + eval e2
eval (Subtract e1 e2)  = eval e1 - eval e2
eval (Multiply e1 e2)  = eval e1 * eval e2 
eval (Divide e1 e2)    = eval e1 `div` eval e2 

readInt :: String -> Int
readInt = read

type ParserType = Parsec [Char] Variables Expression

parseNumber :: ParserType
parseNumber = fmap (Number . readInt) $ many1 digit

parseAdd :: ParserType
parseAdd = do
    string "+ "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Add e1 e2

parseSubtract :: ParserType
parseSubtract = do
    string "- "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Subtract e1 e2

parseMultiply :: ParserType
parseMultiply = do
    string "* "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Multiply e1 e2

parseDivide :: ParserType
parseDivide = do
    string "* "
    e1 <- parseExpression
    char ' '
    e2 <- parseExpression
    return $ Divide e1 e2

parseReadVariable :: ParserType
parseReadVariable = do
    name <- many letter
    st <- getState
    return $ readVariable name st
    

parseExpression :: ParserType
parseExpression =  try parseNumber 
               <|> try parseAdd 
               <|> try parseSubtract 
               <|> try parseMultiply 
               <|> try parseDivide 
               <|> try parseReadVariable 


parseWriteVariable :: Parsec [Char] Variables ()
parseWriteVariable = do
    string "let "
    name <- many letter
    string " = "
    value <- parseExpression
    modifyState $ writeVariable (name, Number (eval value))

command :: ParserType
command = try (parseWriteVariable *> char ';' *> command) <|> parseExpression
    

program = many $ do
    ret <- command
    char ';'
    return ret

a = runParser program Map.empty "" "let x = + 1 5;+ x 2;"

getString :: Either a Expression -> String
getString (Left _) = "Failed"
getString (Right x) = show x





















