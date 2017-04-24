main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Your name :" ++ name
