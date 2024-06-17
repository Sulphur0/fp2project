module Compiler where

import Control.Monad.State
import Data.Bits
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)

type Code   = [String]
type Labels = [(String,Int)]
type Call   = ([Int],[Int])
type Aux    = ([Int],Int)
type Final  = [Int]

tokenize :: String -> Code
tokenize = foldl (++) [] .                      -- concatenate
    map words .                                 -- take all words
        filter (\x -> x/="") .                  -- throw away empty
            map (takeWhile (\x -> x/=';')) .    -- cut them at ;
                lines                           -- take all lines

filterValidCodeLines :: String -> Bool
filterValidCodeLines ""      = False
filterValidCodeLines (';':_) = False
filterValidCodeLines _       = True

isLabel :: String -> Bool
isLabel ""    = False
isLabel (c:_) = c == '.'

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

compileLabels :: String -> Labels
-- we want a map (label, lineid)
-- to achieve that, take all code lines that are labels
-- zip them up with a counting up array that represents valid
-- code line numbers, then from that subtract those code lines
-- that were themselves labels.
compileLabels code = [(label,lineindex-labelcount) | ((label,lineindex),labelcount) <- zip (filter (\(x,_) -> isLabel x) $ zip [takeWhile (not . isSpace) line | line <- lines code, filterValidCodeLines $ trim line] [0..]) [0..]]
-- god help me

getLabel :: String -> Labels -> Int
getLabel ('.':label) known = do
    let proposed = [(validlabel,addr) | (validlabel,addr) <- known, ('.':label) == validlabel]
    if null proposed then
                      -- potentially number label not word label
                      case readMaybe label of
                        Nothing -> error $ "Compiler: Labels: label does not exist: "++ label
                        Just l  -> l
                      else
                      -- this label exists
                      snd $ head proposed
getLabel _ _ = -1

buildCall :: String -> Aux
buildCall "hlt"   = ([0x00],0)
buildCall "mov"   = ([0x01],2)
buildCall "str"   = ([0x02],2)
buildCall "lda"   = ([0x03],1)
buildCall "jmp"   = ([0x04],1)
buildCall "jpz"   = ([0x05],1)
buildCall "jnz"   = ([0x06],1)
buildCall "jpg"   = ([0x07],1)
buildCall "jpl"   = ([0x08],1)
buildCall "add"   = ([0x09],1)
buildCall "sub"   = ([0x0a],1)
buildCall "mul"   = ([0x0b],1)
buildCall "div"   = ([0x0c],1)
buildCall "ret"   = ([0x0d],0)
buildCall "call"  = ([0x0e],1)
buildCall ('.':_) = ([0xff],-1)
buildCall inv     = error $ "Compiler: Parser: invalid call to " ++ inv

createCall :: Code -> State Aux Aux
createCall [] = do
    -- call creation is finished
    (calls,_) <- get
    return (calls,0)
    
createCall (('$':val):next) = do
    -- raw values are flagged with 0x0 so no need to do anything
    (calls,argc) <- get
    let newcall = calls++[read val]
    put (newcall,argc-1)
    createCall next

createCall (('%':_:r:_):next) = do
    -- registers are flagged with 0x1
    (calls,argc) <- get
    let call = head calls
    let appendcall = case r of
                       'a' -> 0x0
                       'b' -> 0x1
                       'c' -> 0x2
                       'd' -> 0x4
                       _ -> error "Compiler: CreateCall: register code does not exist"
    let newcall = (call .|. (shiftL 0x1 (argc*4+4))):(tail calls)++[appendcall] -- add flag to back of call
    put (newcall,argc-1)
    createCall next

createCall (('[':addr):next) = do
    -- memory addresses are flagged with 0x2
    (calls,argc) <- get
    let call = head calls
    let appendcall = read $ takeWhile (\c -> c/=']') addr
    let newcall = (call .|. (shiftL 0x2 (argc*4+4))):(tail calls)++[appendcall] -- add flag to back of call
    put(newcall,argc-1)
    createCall next


createCall (('.':label):next) = do
    -- labels are flagged with 0x4
    (calls,argc) <- get
    let call = head calls
    let newcall = (call .|. (shiftL 0x4 (argc*4+4))):(tail calls) -- add flag to back of call
    case readMaybe label of
      Nothing   -> put (newcall++[0],argc-1)    -- if label is text we'll sort it out later
      Just ladr -> put (newcall++[ladr],argc-1) -- if label is number then we need to keep it
    createCall next

createCall (inv:_) = error $ "Compiler: CreateCall: invalid argument " ++ inv

mergePair :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
mergePair (a0,a1) (b0,b1) = (a0++b0,a1++b1)

compileCodePass :: Code -> Call
compileCodePass [] = ([],[])
compileCodePass (token:rest) = if callsize >= 0 then
                                                -- this code snippet was not a label, compile and add it to compiled stack
                                                mergePair (compiled,[callsize]) (compileCodePass truerest)
                                                else
                                                -- this code snippet was a label, it has no callsize, add flag to compiled stack
                                                mergePair (compiled,[]) (compileCodePass truerest)
    where compiled = fst $ evalState (createCall (take callsize rest)) call
          truerest = drop callsize rest     
          callsize = snd call               
          call = buildCall token            -- array of encoded call and arguments to it

addLabelsPass :: Code -> Call -> Labels -> Final
addLabelsPass [] ([],_) _ = []
addLabelsPass (token:rest) (call:calls,callsizes) labels = 
    case call of
      0xff -> addLabelsPass rest (calls,callsizes) labels
      -- since this code compiled to call was a label, we don't need to put
      -- it on the final stack
      _ -> if laddr < 0 then
            -- there was no label reference as argument, pass the call
            (call:(addLabelsPass rest (calls,callsizes) labels))
            else
            -- thre is a label reference and we need to compute it
            ((laddr + sum (take laddr callsizes):(addLabelsPass rest (calls,callsizes) labels)))

    where laddr = (getLabel token labels)

compile :: String -> Final
compile code = addLabelsPass (tokens) (compileCodePass $ tokens) (compileLabels code)
    where tokens = tokenize code
