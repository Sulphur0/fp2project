module SCompilerCustom where

import Control.Monad.State
import Data.Bits

type FinalStack = [Int]
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
    let ncalls = calls++[read val]
    put (ncalls,argc-1)
    createCallStackElement nargs
createCallStackElement (('.':lab):nargs) = do
    (calls,argc) <- get
    let ncalls = calls++[read lab]
    put (ncalls,argc-1)
    createCallStackElement nargs

initializeCallStackElement :: String -> AuxStack
initializeCallStackElement "hlt" = ([0x00],0)
initializeCallStackElement "mov" = ([0x01],2)
initializeCallStackElement "str" = ([0x02],2)
initializeCallStackElement "lda" = ([0x03],1)
initializeCallStackElement "jmp" = ([0x04],1)
initializeCallStackElement "jpz" = ([0x05],1)
initializeCallStackElement "jnz" = ([0x06],1)
initializeCallStackElement "jpg" = ([0x07],1)
initializeCallStackElement "jpl" = ([0x08],1)
initializeCallStackElement "add" = ([0x09],1)
initializeCallStackElement "sub" = ([0x0A],1)
initializeCallStackElement "mul" = ([0x0B],1)
initializeCallStackElement "div" = ([0x0C],1)

mergePair :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
mergePair (a,b) (c,d) = (a++c,b++d)

compileCodeStack :: CodeStack -> CallStack
compileCodeStack [] = ([],[])
compileCodeStack (token:rest) = (compiledShard,[callsize]) `mergePair` (compileCodeStack truerest)
    where compiledShard = fst $ evalState (createCallStackElement (take callsize rest)) icse
          truerest = drop callsize rest
          callsize = snd icse
          icse = initializeCallStackElement token

getLineLabel :: String -> Int
getLineLabel ('.':label) = read label
getLineLabel _ = -1

calculateLineLabelAddress :: String -> [Int] -> Int
calculateLineLabelAddress token callsizes = n + sum ( take (n) callsizes )
    where n = getLineLabel token

addLineLabelsToCodeStack :: CodeStack -> CallStack -> FinalStack
addLineLabelsToCodeStack [] ([],_) = []
addLineLabelsToCodeStack (token:tokens) (call:calls,callsizes) = if getLineLabel token < 0 then 
                                                                (call:(addLineLabelsToCodeStack tokens (calls,callsizes))) else 
                                                                ((call + sum (take call callsizes)):(addLineLabelsToCodeStack tokens (calls,callsizes)))

compileCode :: String -> FinalStack
compileCode code = addLineLabelsToCodeStack (tokenize code) (compileCodeStack $ tokenize code) 
