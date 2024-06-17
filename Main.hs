import Machine
import Control.Monad.Trans.Class

saveFile :: String -> IO ()
saveFile = writeFile "state.info"

main = do
    input <- getLine
    res <- executeFile $ read input
    putStrLn $ show res
    putStrLn $ "\nSaving machine info to \"state.info\""
    saveFile $ show res

debug = do
    input <- getLine
    (res,log) <- d_executeFile $ read input
    mapM_ putStrLn log
    putStrLn $ "final machine state:"
    putStrLn $ show res
    putStrLn $ "\nSaving machine info to \"state.info\""
    saveFile $ show res
    putStrLn $ "\nSaving logs to \"state.logs\""
    writeFile "state.logs" $ foldl (\x y -> x ++ "\n" ++ y) "LOG START\n" log
