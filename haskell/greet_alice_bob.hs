main = do
    putStrLn "What is your name?"
    name <- getLine
    if (name == "Alice" || name == "Bob")
        then putStrLn ("Hello, " ++ name ++ ".")
        else putStrLn ""
