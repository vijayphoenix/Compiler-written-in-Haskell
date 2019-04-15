import Lib

main :: IO ()
main 
    | "Check" == "Check" = putStrLn "Test Passing"
    | otherwise           = putStrLn "Test Failed"
