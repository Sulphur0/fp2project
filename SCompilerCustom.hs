module SCompilerCustom where

import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List
import Text.Read (readMaybe)

type FinalStack = [Int]
type CallStack = ([Int],[Int])
type LabelStack = [(String,Int)]
type AuxStack = ([Int],Int)
type CodeStack = [String]

tokenize :: String -> CodeStack
tokenize = foldl (++) [] . map words . filter (\x -> x/="") . map (takeWhile (\x -> x/=';')) . lines

createCallStackElement :: CodeStack -> State AuxStack AuxStack

createCallStackElement [] = do
    (calls,_) <- get
    return (calls,0)

createCallStackElement (('%':_:r:_):nargs) = do
    (calls,argc) <- get
    let inscall = head calls    -- modify call code to let it know what is in the next memory address
    let ncall   = case r of
                    'a' -> 0x0
                    'b' -> 0x1
                    'c' -> 0x2
                    'd' -> 0x4
                    _ -> error "Compiler: Parser: register code does not exist"
    let ncalls  = (inscall .|. (0x1 `shiftL` (argc*4+4))):(tail calls)++[ncall]  -- add flag to the back
    put (ncalls,argc-1)
    createCallStackElement nargs

createCallStackElement (('[':addr):nargs) = do
    (calls,argc) <- get
    let inscall = head calls
    let ncall   = read $ takeWhile (\c -> c /= ']') addr
    let ncalls  = (inscall .|. (0x2 `shiftL` (argc*4+4))):(tail calls)++[ncall]
    put (ncalls,argc-1)
    createCallStackElement nargs

createCallStackElement (('$':val):nargs) = do
    (calls,argc) <- get
    let ncalls = calls++[read val]
    put (ncalls,argc-1)
    createCallStackElement nargs

createCallStackElement (('.':lab):nargs) = do
    (calls,argc) <- get
    let inscall = head calls
    let ncalls = (inscall .|. (0x4 `shiftL` (argc*4+4))):(tail calls)
    case readMaybe lab of
      Nothing    -> put (ncalls++[0],argc-1)
      Just label -> put (ncalls++[label],argc-1)
    createCallStackElement nargs

createCallStackElement (inv:_) = error $ "Compiler: Parser: invalid argument " ++ inv

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
initializeCallStackElement ('.':label) = ([0xFF],-1)
initializeCallStackElement inv   = error $ "Compiler: Parser: invalid call to " ++ inv

mergePair :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
mergePair (a,b) (c,d) = (a++c,b++d)

compileCodeStack :: CodeStack -> CallStack
compileCodeStack [] = ([],[])
compileCodeStack (token:rest) = if callsize >=0 then (compiledShard,[callsize]) `mergePair` (compileCodeStack truerest)
                                                else (compiledShard,[]) `mergePair` (compileCodeStack truerest)     -- this means we hit a label
    where compiledShard = fst $ evalState (createCallStackElement (take callsize rest)) icse
          truerest = drop callsize rest
          callsize = snd icse
          icse = initializeCallStackElement token

getLineLabel :: String -> LabelStack -> Int
getLineLabel ('.':label) labels = do
    let proposedLabel = [(ll,addr) | (ll,addr)<-labels, ('.':label) == ll]
    if proposedLabel == [] then
            read label
            else
            snd $ head proposedLabel
getLineLabel _ _ = -1

addLineLabelsToCodeStack :: CodeStack -> Int -> CallStack -> LabelStack -> FinalStack
addLineLabelsToCodeStack [] _ ([],_) _ = []
addLineLabelsToCodeStack (token:tokens) tokencount (call:calls,callsizes) labels = case call of
                        -- this is a label
                        0xFF -> addLineLabelsToCodeStack tokens tokencount (calls,callsizes) labels
                        _ -> if getLineLabel token labels < 0 then 
                                                       (call:(addLineLabelsToCodeStack tokens tokencount (calls,callsizes) labels)) else 
                                                       (((labelcall) + sum (take labelcall callsizes)):(addLineLabelsToCodeStack tokens tokencount (calls,callsizes) labels))
                                                           where labelcall = getLineLabel token labels




trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

isLabel :: String -> Bool
isLabel "" = False
isLabel (c:_) = c=='.'

filterValidCodeLines :: String -> Bool
filterValidCodeLines "" = False
filterValidCodeLines (';':_) = False
filterValidCodeLines _ = True

compileLabels :: String -> LabelStack
compileLabels code = [ (l,i-j) | ((l,i),j)<-zip (filter (\(x,_) -> isLabel x) $ zip [takeWhile (not . isSpace) line | line <- lines code, filterValidCodeLines $ trim line] [0..]) [0..]]

compileCode :: String -> FinalStack
compileCode code = addLineLabelsToCodeStack (tokens) (length $ tokens) (compileCodeStack $ tokens) (compileLabels code) -- (scanLabels tokens length $ tokens)
    where tokens = tokenize code
