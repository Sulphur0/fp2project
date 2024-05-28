import SCompilerCustom
import Custom

main = do
    input <- getLine
    executeFile $ read input
