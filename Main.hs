import Machine

main = do
    input <- getLine
    executeFile $ read input
