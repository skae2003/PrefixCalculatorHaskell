import Data.Char (isSpace, isDigit)

parseNumber :: String -> Maybe (Int, String)
parseNumber expr = case span isDigit expr of
  ("", _)    -> Nothing
  (digits, rest) -> Just (read digits, rest)

parseOperation :: (Int -> Int -> Int) -> String -> [Int] -> Maybe (Int, String)
parseOperation _ [] _ = Nothing
parseOperation operation expr history = do
  (val1, remainingExpr) <- evalExpr expr history
  (val2, newRemainingExpr) <- evalExpr remainingExpr history
  return (operation val1 val2, newRemainingExpr)

evalExpr :: String -> [Int] -> Maybe (Int, String)
evalExpr [] _ = Nothing
evalExpr (c:rest) history
  | isSpace c = evalExpr rest history
  | isDigit c = parseNumber (c:rest)
  | c `elem` "+*" = parseOperation (if c == '+' then (+) else (*)) rest history
  | c == '-' = do
      (operand, remainingExpr) <- evalExpr rest history
      return (-operand, remainingExpr)
  | c == '/' = do
      (val1, remainingExpr) <- evalExpr rest history
      if val1 == 0
        then fail "Division by zero"
        else do
          (val2, newRemainingExpr) <- evalExpr remainingExpr history
          return (val1 `div` val2, newRemainingExpr)
  | c == '$' = do
      (id, remainingExpr) <- parseNumber rest
      let index = length history - id
      if index < 0 || index >= length history
        then fail "Invalid index for $ operator"
        else Just (history !! index, remainingExpr)
  | otherwise = fail "Invalid expression"

evalLoop :: [Int] -> IO ()
evalLoop history = do
  putStrLn "Enter an expression in prefix notation (ex: '+ 2 2', '-5', '+ $1 2' or 'exit' to exit): "
  input <- getLine
  if input == "exit"
    then putStrLn "Exiting..."
    else case evalExpr input history of
      Nothing -> putStrLn "Error: Invalid expression" >> evalLoop history
      Just (value, remainingExpr)
        | not (null remainingExpr) -> putStrLn "Error: Trailing characters." >> evalLoop history
        | otherwise -> do
            let historyLength = length history + 1
                valueStr = show value
            putStrLn (show historyLength ++ ": " ++ valueStr)
            evalLoop (value : history)

main :: IO ()
main = evalLoop []