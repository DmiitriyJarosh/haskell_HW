module Parser where


import Eval
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Foldable as Foldable

translateReset :: Term -> Term
translateReset (Reset term) = let (body, logTerm) = preEval term in
  case logTerm of
    Shift name bodyShift -> TmRst (TmAbs name body) name (translateReset bodyShift)
    TmCst x -> body
translateReset term = case term of
  TmVar x -> term
  TmCst x -> term
  Nil -> term
  Ycomb -> term
  TmAbs name body -> TmAbs name (translateReset body)
  TmAdd terml termr -> TmAdd (translateReset terml) (translateReset termr)
  TmApp terml termr -> TmApp (translateReset terml) (translateReset termr)
  TmMult terml termr -> TmMult (translateReset terml) (translateReset termr)
  Shift name body -> TmApp (TmCst 0) (TmCst 0) --to call mistake, no time to do it normally
  TmRst func name body -> TmRst func name (translateReset body)
  IfThenElse cond termT termF -> IfThenElse (translateReset cond) (translateReset termT) (translateReset termF)
  TmList body listTail -> TmList (translateReset body) (translateReset listTail)
  Head body -> Head (translateReset body)
  Tail body -> Tail (translateReset body)
  IsNil body -> IsNil (translateReset body)

addT :: Term -> Term -> Term
addT x y = case x of
  Shift name body -> x
  otherwise -> y

preEval :: Term -> (Term, Term)
preEval term = case term of
  TmVar x -> (term, TmCst 0)
  Nil -> (term, TmCst 0)
  Ycomb -> (term, TmCst 0)
  TmCst x -> (term, TmCst 0)
  TmAbs name body -> (TmAbs name (fst $ preEval body), snd $ preEval body)
  TmApp terml termr -> (TmApp (fst $ preEval terml) (fst $ preEval termr), addT (snd $ preEval terml) (snd $ preEval termr))
  TmAdd terml termr -> (TmAdd (fst $ preEval terml) (fst $ preEval termr), addT (snd $ preEval terml) (snd $ preEval termr))
  TmMult terml termr -> (TmMult (fst $ preEval terml) (fst $ preEval termr), addT (snd $ preEval terml) (snd $ preEval termr))
  Reset body -> preEval $ translateReset term
  Shift name body -> (TmVar name, Shift name body)
  TmRst fun name body -> (TmRst fun name (fst $ preEval body), snd $ preEval body)
  IfThenElse termCond termThen termElse ->
      (IfThenElse (translateReset termCond) (translateReset termThen) (translateReset termElse), TmCst 0)
  IsNil term -> (IsNil (fst $ preEval term), snd $ preEval term)
  Head term -> (Head (fst $ preEval term), snd $ preEval term)
  Tail term -> (Tail (fst $ preEval term), snd $ preEval term)
  TmList body listTail ->
      (TmList (translateReset body) (translateReset listTail), TmCst 0)


languageDef = emptyDef {
                        Token.identStart = letter,
                        Token.identLetter = alphaNum,
                        Token.reservedOpNames = ["+", "-", ",", "$","*"],
                        Token.reservedNames = ["\\", "->", "Y", "reset", "shift", "[", "]", "isNil", "head", "tail", "if", "then", "else"]
                       }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
symbol = Token.symbol lexer
brackets = Token.brackets lexer

mainParser :: Parser Term
mainParser = whiteSpace >> expression



expression :: Parser Term
expression = buildExpressionParser operators term

operators = [
              [Infix (reservedOp "$" >> return TmApp) AssocLeft],
              [Infix (reservedOp "+" >> return TmAdd) AssocLeft],
              [Infix (reservedOp "*" >> return TmMult) AssocLeft],
              [Infix (reservedOp "," >> return TmList) AssocRight]
            ]



term :: Parser Term
term = listParse <|> nilParse <|> isNilParse <|> combYParse <|> headParse <|> tailParse <|> resetParse <|> shiftParse <|> lambdaParse <|> parens expression <|> ifThenElseParse <|> fmap TmVar identifier <|> fmap TmCst integer

listParse :: Parser Term
listParse = brackets expression

nilParse :: Parser Term
nilParse = do
           reserved "nil"
           return Nil

combYParse :: Parser Term
combYParse = do
           reserved "Y"
           return Ycomb

isNilParse :: Parser Term
isNilParse = do
             reserved "isNil"
             body <- expression
             return $ IsNil body

headParse :: Parser Term
headParse = do
            reserved "head"
            body <- expression
            return $ Head body

tailParse :: Parser Term
tailParse = do
            reserved "tail"
            body <- expression
            return $ Tail body

shiftParse :: Parser Term
shiftParse = do
             reserved "shift"
             whiteSpace
             nameVar <- identifier
             whiteSpace
             body <- expression
             return $ Shift nameVar body


ifThenElseParse :: Parser Term
ifThenElseParse = do
          reserved "if"
          cond <- expression
          reserved "then"
          bodyThen <- expression
          reserved "else"
          bodyElse <- expression
          return $ IfThenElse cond bodyThen bodyElse

resetParse :: Parser Term
resetParse = do
             reserved "reset"
             resetInParens <|> parens resetInParens


resetInParens :: Parser Term
resetInParens = do
                body <- expression
                return $ Reset body

lambdaParse :: Parser Term
lambdaParse = do
         reserved "\\"
         nameVar <- identifier
         reserved "->"
         body <- expression
         return $ TmAbs nameVar body


-----------Adding nil to end of lists in string

findEndOfList :: (String, String) -> (String, String)
findEndOfList (str,x:xs) = if x == ']' then findEndOfList (str ++ ",nil" ++ [x],xs) else findEndOfList (str ++ [x], xs)
findEndOfList (str, []) = (str,[])

forNilList :: (String,String) -> (String, String)
forNilList (str, x:(x2:xs)) = if (x == '[') && (x2 == ',') then forNilList (str ++ [x], xs) else forNilList (str ++ [x], x2:xs)
forNilList (str, [x]) = (str ++ [x], [])

transformString :: String -> String
transformString str = fst $ forNilList ([], fst (findEndOfList ([],str)))

---------------------------------------


parseString :: String -> Term
parseString str = case parse mainParser "" (transformString str) of
                    Left e -> error $ show e
                    Right r -> r

myParse str = translateReset $ parseString str
testY = "(\\ f -> (\\ x -> f$(x$x))$(\\ x -> f$(x$x)))"
--fac' = (\\ fun -> (\\ n -> if n then (n*(fun$(n-1))) else 1))
test0Y = "(Ycomb$(\\ fun -> (\\ n -> if n then (n*(fun$(n+(-1)))) else 1)))$3"
test0 = "((\\ f -> (\\ x -> f$(x$x))$(\\ x -> f$(x$x)))$(\\ fun -> (\\ n -> if n then (n*(fun$(n+(-1)))) else 1)))$3"
test1 = "1+reset(1+(shift k k$2))" -- == 1+(+1)2 == 4
test2 = "reset(1+(shift x (x$1)))" -- == 2
test3 = "(1+1)+reset(1+(shift x (x$1)))" -- == 4
test4 = "(reset(1+(shift x x)))$(1+1)" -- == 2
test5 = "(\\ x -> x + 1) $ 5" -- == 6
test6 = "(1+1)$1" --error
test7 = "reset ([1,2,(shift k (k$2))])"
test8 = "shift k (k$2)"
test9 = "reset (if shift k (k$2) then 6 else 3)"
test10 = "reset (if (shift k k$4) then (shift k k$6) else (shift q q$0))"
test101 = "(if (shift k k$4) then (shift k k$6) else (shift q q$0))"
