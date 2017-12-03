-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn $ "Hey " ++ name ++ ", you rock!"


main = do
    string <- getLine
    if null string
        then return ()
        else do
            putStrLn $ reverse string
            main

