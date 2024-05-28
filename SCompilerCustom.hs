module SCompilerCustom where

import Control.Monad.State
import Data.Bits

type CallStack = ([Int],[Int])
type AuxStack = ([Int],Int)
type CodeStack = [String]

tokenize :: String -> CodeStack
tokenize = foldl (++) [] . map words . lines

createCallStackElement :: CodeStack -> State AuxStack AuxStack
createCallStackElement [] = do
    (calls,_) <- get
    return (calls,0)
createCallStackElement (('%':_:r:_):nargs) = do
    (calls,argc) <- get
    let inscall = head calls
    let ncall = case r of
                  'a' -> 0x0
                  'b' -> 0x1
                  'c' -> 0x2
                  'd' -> 0x3
                  -- _  -> 0xF
    let ncalls = (inscall .|. (0x1 `shiftL` (argc*4+4))):(tail calls)++[ncall]
    put (ncalls,argc-1)
    createCallStackElement nargs
createCallStackElement (('[':addr):nargs) = do
    (calls,argc) <- get
    let inscall = head calls
    let ncall = read $ takeWhile (\c -> c /= ']') addr
    let ncalls = (inscall .|. (0x2 `shiftL` (argc*4+4))):(tail calls)++[ncall]
    put (ncalls,argc-1)
    createCallStackElement nargs
createCallStackElement (('$':val):nargs) = do
    (calls,argc) <- get
    let ncall = read val
    let ncalls = calls++[ncall]
    put (ncalls,argc-1)
    createCallStackElement nargs

initializeCallStackElement :: String -> AuxStack
initializeCallStackElement "hlt" = ([0X00],0)
initializeCallStackElement "mov" = ([0X01],2)
initializeCallStackElement "str" = ([0X02],2)
initializeCallStackElement "jmp" = ([0X03],1)
initializeCallStackElement "jpz" = ([0X04],1)
initializeCallStackElement "jnz" = ([0X05],1)
initializeCallStackElement "jpg" = ([0X06],1)
initializeCallStackElement "jpl" = ([0X07],1)
initializeCallStackElement "add" = ([0X08],1)
initializeCallStackElement "sub" = ([0X09],1)
initializeCallStackElement "mul" = ([0X0A],1)
initializeCallStackElement "div" = ([0X0B],1)

mergePair :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
mergePair (a,b) (c,d) = (a++c,b++d)

compileCodeStack :: CodeStack -> CallStack
compileCodeStack [] = ([],[])
compileCodeStack (token:rest) = (compiledShard,[n]) `mergePair` (compileCodeStack truerest)
    where compiledShard = fst $ evalState (createCallStackElement (take n rest)) icse
          truerest = drop n rest
          n = snd icse
          icse = initializeCallStackElement token

compileCode :: String -> CallStack
compileCode = compileCodeStack . tokenize
