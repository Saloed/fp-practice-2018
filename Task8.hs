module Task8 where

import Text.Parsec hiding ( digit )

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf $ ['0' .. '9']

numberStr :: Parser String
numberStr = many1 digit

intNumber :: Parser Float
intNumber = read <$> numberStr

parseFracPart :: Parser String
parseFracPart = do
  char '.'
  fracPart <- numberStr
  return $ ('.' : fracPart)

floatNumber :: Parser Float
floatNumber = do
  numb    <- numberStr
  decimal <- option "" parseFracPart
  return $ read $ (numb ++ decimal)

number :: Parser Float
number = floatNumber

byNumber
  :: Char
  -> (Float -> Float -> Float)
  -> Parser Float
  -> Parser (Float -> Float)
byNumber symbol func base = do
  char symbol
  spaces
  n <- base
  spaces
  return $ (`func` n)


powerNumber :: Parser (Float -> Float)
powerNumber = byNumber '^' (**) minusedExpr

multNumber :: Parser (Float -> Float)
multNumber = byNumber '*' (*) power

divNumber :: Parser (Float -> Float)
divNumber = byNumber '/' (/) power

plusNumber :: Parser (Float -> Float)
plusNumber = byNumber '+' (+) multiplication

minusNumber :: Parser (Float -> Float)
minusNumber = byNumber '-' (-) multiplication

power :: Parser Float
power = do
  x <- minusedExpr
  spaces
  ys <- many powerNumber
  return $ foldl (\x f -> f x) x ys

multiplication :: Parser Float
multiplication = do
  x <- power
  spaces
  ys <- many (multNumber <|> divNumber)
  return $ foldl (\x f -> f x) x ys

addition :: Parser Float
addition = do
  x <- multiplication
  spaces
  ys <- many (plusNumber <|> minusNumber)
  return $ foldl (\x f -> f x) x ys

minusedExpr :: Parser Float
minusedExpr = expr <|> do
  char '-'
  spaces
  res <- expr
  return $ negate res

expr :: Parser Float
expr = number <|> do
  char '('
  spaces
  res <- addition
  char ')'
  spaces
  return $ res

root :: Parser Float
root = do
  spaces
  p <- addition
  eof
  return $ p

main = do
  s <- getLine
  putStrLn $ show $ parse root "<input>" s
  main
