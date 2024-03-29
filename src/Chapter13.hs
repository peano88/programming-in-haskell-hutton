module Chapter13 where
import Parser ( Parser, parse, sat, natural, symbol )
import Control.Applicative ( Alternative((<|>), many) )

comment :: Parser ()
comment = do
  symbol "--"
  many (sat (/= '\n'))
  return ()

data ArithmExpression = Simple Int | Brackets ArithmExpression | Sum ArithmExpression ArithmExpression | Prod ArithmExpression ArithmExpression

evalExpr :: ArithmExpression -> Int
evalExpr (Simple   n ) = n
evalExpr (Brackets ae) = evalExpr ae
evalExpr (Sum  l r   ) = evalExpr l + evalExpr r
evalExpr (Prod l r   ) = evalExpr l * evalExpr r

sumParser :: Parser ArithmExpression
sumParser = do
  l <- prodParser
  do
      symbol "+"
      Sum l <$> sumParser
    <|> return l

prodParser :: Parser ArithmExpression
prodParser = do
  l <- factorParser
  do
      symbol "*"
      Prod l <$> prodParser
    <|> return l

factorParser :: Parser ArithmExpression
factorParser =
  do
      symbol "("
      v <- sumParser
      symbol ")"
      return (Brackets v)
    <|> do Simple <$> natural


parseEval :: String -> Int
parseEval xs = case parse sumParser xs of
  [(ae, "" )] -> evalExpr ae
  [(_ , out)] -> error ("Invalid input: unused: " ++ out)
  []          -> error "invalid input"
