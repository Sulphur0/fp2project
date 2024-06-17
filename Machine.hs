module Machine where

import Data.Bits
import Numeric (showHex)
import Control.Monad.Writer
import Compiler

type Reg = (Int,Int,Int,Int)
type Mem = [Int]
type PC  = Int
type Sta = [Int]
data Machine = Run (PC,Reg,Mem,Sta) | Halt (PC,Reg,Mem,Sta)
----------------------------------------------------------------------------
-- THIS IMPLEMENTATION IS VOMIT INDUCING I WILL NOT LOOK AT IT ANY LONGER --
----------------------------------------------------------------------------
fixedlength :: Int -> Int -> String -> String
fixedlength n lim [] = [' ' | _<-[n..lim]]
fixedlength n lim (c:cs) = if lim == n then [] else c:(fixedlength (n+1) lim cs)

splitchunks :: Int -> [Int] -> String
splitchunks n [] = []
splitchunks n x  = "0x"++(fixedlength 0 3 (showHex (n*8) "")) ++ ":  " ++ (tail $ foldl (\x y -> x ++ " " ++ y) "" (map (\x -> "0x" ++ (fixedlength 0 4 $ showHex x "")) (take 8 x))) ++ "\n" ++ (splitchunks (n+1) (drop 8 x))

array2str :: Show a => [a] -> String
array2str [] = "empty"
array2str x  = foldl (\x y -> x++","++y) "" (map show x)

instance Show Machine where
    show (Run (pc,(a,b,c,d),mem,sta)) = "program counter: " ++ (show pc) ++ "\nregisters: " ++ "\n\ta: " ++ (show a) ++ "\n\tb: " ++ (show b) ++ "\n\tc: " ++ (show c) ++ "\n\td: " ++ (show d) ++ "\nmemory:\n" ++ (splitchunks 0 mem) ++ "\nstack: " ++ (array2str sta)
    show (Halt x) = show (Run x)
---------------------
-- I AM VERY SORRY --
---------------------

type Instruction = (Machine -> Int -> Machine)

maxmem :: Int
maxmem = 100

envinit :: Machine
envinit = Run (0, (0,0,0,0), [0 | _<-[1..maxmem]], [])

envhalt :: Machine -> Machine
envhalt (Run (pc,reg,mem,sta)) = Halt (pc,reg,mem,sta)
envhalt x = x

memget :: Mem -> Int -> Int
memget m p   = if p > maxmem then error $ "Interpreter: get mem: pointer "++(show p)++" out of bounds"
                           else m !! p

memset :: Mem -> Int -> Int -> Mem
memset m p v = if p > maxmem then error $ "Interpreter: set mem: pointer "++(show p)++" out of bounds"
                             else (take p m) ++ [v] ++ (drop (p+1) m)

regget :: Reg -> Int -> Int
regget (r,_,_,_) 0x0 = r
regget (_,r,_,_) 0x1 = r
regget (_,_,r,_) 0x2 = r
regget (_,_,_,r) 0x4 = r
regget _ inv         = error $ "Interpreter: get register: Invalid register address: " ++ (show inv)

regset :: Reg -> Int -> Int -> Reg
regset (a,b,c,d) 0x0 v = (v,b,c,d)
regset (a,b,c,d) 0x1 v = (a,v,c,d)
regset (a,b,c,d) 0x2 v = (a,b,v,d)
regset (a,b,c,d) 0x4 v = (a,b,c,v)
regset _ inv _         = error $ "Interpreter: set register: Invalid register address: " ++ (show inv)

stapush :: Sta -> Int -> Sta
stapush s a = a:s

stapop :: Sta -> (Sta,Int)
stapop [] = error $ "Interpreter: pop stack: empty stack"
stapop (a:s) = (s,a)

type CallCmd = (Int -> Machine -> Machine)

-- master call
call :: CallCmd

-- auxilary functions
_jump_conditional :: Machine -> (Int -> Int -> Bool) -> Int -> Machine
_jump_conditional (Run (pc,(a,b,c,d),m,st)) cond to = if cond a 0 then
                                                             (Run (to,(a,b,c,d),m,st))
                                                             else
                                                             (Run (pc+2,(a,b,c,d),m,st))

_arithmetic :: Machine -> (Int -> Int -> Int) -> Int -> Machine
_arithmetic (Run (pc,(a,b,c,d),m,st)) op v = (Run (pc+2,(op a v,b,c,d),m,st))

c_hlt  :: CallCmd
c_mov  :: CallCmd
c_lda  :: CallCmd
c_str  :: CallCmd
c_jmp  :: CallCmd
c_jpz  :: CallCmd
c_jnz  :: CallCmd
c_jpg  :: CallCmd
c_jpl  :: CallCmd
c_add  :: CallCmd
c_sub  :: CallCmd
c_mul  :: CallCmd
c_div  :: CallCmd
c_ret  :: CallCmd
c_call :: CallCmd

c_hlt _ (Run x) = (Halt x)
c_hlt _ x = x

c_mov cmd (Run (pc,r,m,st)) = case mask12 of
                                0x1 -> case mask8 of
                                         0x1 -> (Run (pc+3,regset r set $ regget r get,m,st))
                                         0x2 -> (Run (pc+3,r,memset m set $ regget r get,st))
                                         inv -> error $ "Interpreter: c_mov: invalid flag " ++ (show inv)
                                0x2 -> case mask8 of
                                         0x1 -> (Run (pc+3,regset r set $ memget m get,m,st))
                                         inv -> error $ "Interpreter: c_mov: invalid flag " ++ (show inv)
                                inv -> error $ "Interpreter: c_mov: invalid flag " ++ (show inv)

    where mask8  = cmd `shiftR` 8 .&. 0xf
          mask12 = cmd `shiftR` 12 .&. 0xf
          get    = (memget m (pc+1))    -- source address
          set    = (memget m (pc+2))    -- destination address

c_lda cmd (Run (pc,r,m,st)) = case mask8 of
                                        0x0 -> (Run (pc+2,regset r 0   get,m,st))
                                        0x1 -> (Run (pc+2,regset r 0 $ regget r get,m,st))
                                        0x2 -> (Run (pc+2,regset r 0 $ memget m get,m,st))
                                        inv -> error $ "Interpreter: c_lda: invalid flag " ++ (show inv)

    where mask8 = cmd `shiftR` 8 .&. 0xf
          get   = (memget m (pc+1))

c_str cmd (Run (pc,r,m,st)) = if and [mask12 == 0x0,mask8 == 0x2] then (Run (pc+3,r,memset m set get,st))
                                                                  else error $ "Interpreter: c_str: invalid argument(s)"

    where mask8  = cmd `shiftR` 8 .&. 0xf
          mask12 = cmd `shiftR` 12 .&. 0xf
          get    = (memget m (pc+1))
          set    = (memget m (pc+2))

c_jmp cmd (Run (pc,r,m,st)) = (Run (memget m (pc+1),r,m,st))

c_jpz _ (Run (pc,r,m,st)) = _jump_conditional (Run (pc,r,m,st)) (==) (memget m (pc+1))
c_jnz _ (Run (pc,r,m,st)) = _jump_conditional (Run (pc,r,m,st)) (/=) (memget m (pc+1))
c_jpg _ (Run (pc,r,m,st)) = _jump_conditional (Run (pc,r,m,st)) (>)  (memget m (pc+1))
c_jpl _ (Run (pc,r,m,st)) = _jump_conditional (Run (pc,r,m,st)) (<)  (memget m (pc+1))

c_add cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> _arithmetic (Run (pc,r,m,st)) (+) (memget m (pc+1))
                                0x2 -> _arithmetic (Run (pc,r,m,st)) (+) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_add: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf
c_sub cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> _arithmetic (Run (pc,r,m,st)) (-) (memget m (pc+1))
                                0x2 -> _arithmetic (Run (pc,r,m,st)) (-) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_sub: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf
c_mul cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> _arithmetic (Run (pc,r,m,st)) (*) (memget m (pc+1))
                                0x2 -> _arithmetic (Run (pc,r,m,st)) (*) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_mul: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf
c_div cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> _arithmetic (Run (pc,r,m,st)) (div) (memget m (pc+1))
                                0x2 -> _arithmetic (Run (pc,r,m,st)) (div) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_div: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf

c_ret cmd (Run (pc,r,m,st)) = (Run (jto,r,m,pst))
    where (pst,jto) = stapop st

c_call cmd (Run (pc,r,m,st)) = (Run (memget m (pc+1),r,m,stapush st (pc+2)))


call cmd = case mask of
                 0x00 -> c_hlt cmd
                 0x01 -> c_mov cmd
                 0x02 -> c_str cmd
                 0x03 -> c_lda cmd
                 0x04 -> c_jmp cmd
                 0x05 -> c_jpz cmd
                 0x06 -> c_jnz cmd
                 0x07 -> c_jpg cmd
                 0x08 -> c_jpl cmd
                 0x09 -> c_add cmd
                 0x0a -> c_sub cmd
                 0x0b -> c_mul cmd
                 0x0c -> c_div cmd
                 0x0d -> c_ret cmd
                 0x0e -> c_call cmd
                 inv -> error $ "Interpreter: call: received invalid call code " ++ (show inv)
    where mask = cmd .&. 0xff

-- simulating machine

execute :: Machine -> Machine
execute (Halt x) = Halt x
execute (Run (pc,r,m,st)) = execute $ call (m !! pc) (Run (pc,r,m,st))

loadCode :: Machine -> Final -> Machine
loadCode (Run (pc,r,m,st)) code = if length code > maxmem then error $ "Interpreter: loadCode: code is too long, " ++ (show $ length code) ++ "/" ++ (show maxmem)
                                                             else (Run (pc,r,code ++ drop (length code) m, st))

printLoadedFile filename = do
    content <- readFile filename
    let memoryImage = loadCode envinit (compile $ content)
    return memoryImage

executeFile filename = do
    content <- readFile filename
    let halted = execute $ loadCode envinit (compile $ content)
    return halted

--------------------------
--      DEBUGGING       --
--------------------------
-- everything here is a rehash of above code
-- with the addition of the Writer monad
-- which contains all logs of execution of 
-- machine code

d_cmdname :: Int -> String
d_cmdname cmd = case mask of
                 0x00 -> "hlt"
                 0x01 -> "mov"
                 0x02 -> "str"
                 0x03 -> "lda"
                 0x04 -> "jmp"
                 0x05 -> "jpz"
                 0x06 -> "jnz"
                 0x07 -> "jpg"
                 0x08 -> "jpl"
                 0x09 -> "add"
                 0x0a -> "sub"
                 0x0b -> "mul"
                 0x0c -> "div"
                 0x0d -> "ret"
                 0x0e -> "call"
                 inv -> error $ "Debug: d_cmdname: received invalid call code " ++ (show inv)
    where mask = cmd .&. 0xff

d_execute :: Machine -> Writer [String] Machine
d_execute (Halt (pc,r,m,st)) = do
    tell ["halted at 0x" ++ (showHex pc "") ++ "."]
    return (Halt (pc,r,m,st))
d_execute (Run (pc,r,m,st)) = do
    let common = "at 0x" ++ (showHex pc "") ++ ": executing " ++ (d_cmdname $ m !! pc)
    tell [common]
    (machine,log) <- listen $ d_call (m !! pc) (Run (pc,r,m,st))
    continue <- d_execute machine
    return continue

d_logs :: Machine -> (Machine, [String])
d_logs initial = runWriter $ d_execute initial

d_executeFile filename = do
    content <- readFile filename
    let halted = d_logs $ loadCode envinit (compile $ content)
    return halted

type DCallCmd = (Int -> Machine -> Writer [String] Machine)

d_reggetname :: Int -> String
d_reggetname 0x0 = "%ra"
d_reggetname 0x1 = "%rb"
d_reggetname 0x2 = "%rc"
d_reggetname 0x4 = "%rd"

d_memshow :: Int -> String
d_memshow n = "[" ++ (show n) ++ "]"

-- master call
d_call :: DCallCmd

-- auxilary functions
_djump_conditional :: Machine -> (Int -> Int -> Bool) -> Int -> Writer [String] Machine
_djump_conditional (Run (pc,(a,b,c,d),m,st)) cond to = if cond a 0 then do
                                                                  tell [" condition met, jumping to 0x" ++ (showHex to "")]
                                                                  return (Run (to,(a,b,c,d),m,st))
                                                             else do
                                                                 tell [" condition not met, continuing"]
                                                                 return (Run (pc+2,(a,b,c,d),m,st))

_darithmetic :: Machine -> (Int -> Int -> Int) -> Int -> Writer [String] Machine
_darithmetic (Run (pc,(a,b,c,d),m,st)) op v = do
    tell [" %ra was " ++ (show a) ++ ", now " ++ (show $ op a v)]
    return (Run (pc+2,(op a v,b,c,d),m,st))

dc_hlt  :: DCallCmd
dc_mov  :: DCallCmd
dc_lda  :: DCallCmd
dc_str  :: DCallCmd
dc_jmp  :: DCallCmd
dc_jpz  :: DCallCmd
dc_jnz  :: DCallCmd
dc_jpg  :: DCallCmd
dc_jpl  :: DCallCmd
dc_add  :: DCallCmd
dc_sub  :: DCallCmd
dc_mul  :: DCallCmd
dc_div  :: DCallCmd
dc_ret  :: DCallCmd
dc_call :: DCallCmd

dc_hlt _ (Run x) = do
    tell ["halting..."]
    return (Halt x)
dc_hlt _ x = do
    tell ["already halted!"]
    return x

dc_mov cmd (Run (pc,r,m,st)) = case mask12 of
                                0x1 -> case mask8 of
                                         0x1 -> do
                                             tell [ "moving " ++ (show $ regget r get) ++ " from register " ++ (d_reggetname get) ++ " to register " ++ (d_reggetname set)]
                                             return (Run (pc+3,regset r set $ regget r get,m,st))
                                         0x2 -> do
                                             tell [" moving " ++ (show $ regget r get) ++ " from register " ++ (d_reggetname get) ++ " to address " ++ (d_memshow set)]
                                             return (Run (pc+3,r,memset m set $ regget r get,st))
                                         inv -> error $ "Interpreter: c_mov: invalid flag " ++ (show inv)
                                0x2 -> case mask8 of
                                         0x1 -> do
                                             tell [" moving " ++ (show $ memget m get) ++ " from address " ++ (d_memshow get) ++ " to register " ++ (d_reggetname set)]
                                             return (Run (pc+3,regset r set $ memget m get,m,st))
                                         inv -> error $ "Interpreter: c_mov: invalid flag " ++ (show inv)
                                inv -> error $ "Interpreter: c_mov: invalid flag " ++ (show inv)

    where mask8  = cmd `shiftR` 8 .&. 0xf
          mask12 = cmd `shiftR` 12 .&. 0xf
          get    = (memget m (pc+1))    -- source address
          set    = (memget m (pc+2))    -- destination address

dc_lda cmd (Run (pc,r,m,st)) = case mask8 of
                                        0x0 -> do
                                            tell [" loading value " ++ (show get) ++ " into accumulator %ra"]
                                            return (Run (pc+2,regset r 0   get,m,st))
                                        0x1 -> do
                                            tell [" loading value " ++ (show $ regget r get) ++ " from register " ++ (d_reggetname get) ++ " into accumulator %ra"]
                                            return (Run (pc+2,regset r 0 $ regget r get,m,st))
                                        0x2 -> do
                                            tell [" loading value " ++ (show $ memget m get) ++ " from address " ++ (d_memshow get) ++ " into accumulator %ra"]
                                            return (Run (pc+2,regset r 0 $ memget m get,m,st))
                                        inv -> error $ "Interpreter: c_lda: invalid flag " ++ (show inv)

    where mask8 = cmd `shiftR` 8 .&. 0xf
          get   = (memget m (pc+1))

dc_str cmd (Run (pc,r,m,st)) = if and [mask12 == 0x0,mask8 == 0x2] then do
                                                                   tell [" storing value " ++ (show get) ++ " into address " ++ (d_memshow set)]
                                                                   return (Run (pc+3,r,memset m set get,st))
                                                                  else error $ "Interpreter: c_str: invalid argument(s)"

    where mask8  = cmd `shiftR` 8 .&. 0xf
          mask12 = cmd `shiftR` 12 .&. 0xf
          get    = (memget m (pc+1))
          set    = (memget m (pc+2))

dc_jmp cmd (Run (pc,r,m,st)) = do
    tell [" jumping unconditionally to address 0x" ++ (showHex (memget m (pc+1)) "")]
    return (Run (memget m (pc+1),r,m,st))

dc_jpz _ (Run (pc,r,m,st)) = do
    tell [" testing if accumulator %ra is 0"]
    _djump_conditional (Run (pc,r,m,st)) (==) (memget m (pc+1))
dc_jnz _ (Run (pc,r,m,st)) = do
    tell [" testing if accumulator %ra is not 0"]
    _djump_conditional (Run (pc,r,m,st)) (/=) (memget m (pc+1))
dc_jpg _ (Run (pc,r,m,st)) = do
    tell [" testing if accumulator %ra is greater than 0"]
    _djump_conditional (Run (pc,r,m,st)) (>)  (memget m (pc+1))
dc_jpl _ (Run (pc,r,m,st)) = do
    tell [" testing if accumulator %ra is less than 0"]
    _djump_conditional (Run (pc,r,m,st)) (<)  (memget m (pc+1))

dc_add cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> do
                                    tell [" adding value " ++ (show $ memget m (pc+1)) ++ " into accumulator %ra"]
                                    _darithmetic (Run (pc,r,m,st)) (+) (memget m (pc+1))
                                0x2 -> do
                                    tell [" adding value " ++ (show $ memget m (memget m (pc+1))) ++ " from address " ++ (d_memshow $ memget m (pc+1)) ++ " into accumulator %ra"]
                                    _darithmetic (Run (pc,r,m,st)) (+) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_add: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf
dc_sub cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> do
                                    tell [" subtracting value " ++ (show $ memget m (pc+1)) ++ " from accumulator %ra"]
                                    _darithmetic (Run (pc,r,m,st)) (-) (memget m (pc+1))
                                0x2 -> do
                                    tell [" subtracting value " ++ (show $ memget m (memget m (pc+1))) ++ " from address " ++ (d_memshow $ memget m (pc+1)) ++ " from accumulator %ra"]
                                    _darithmetic (Run (pc,r,m,st)) (-) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_sub: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf
dc_mul cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> do
                                    tell [" multiplying value " ++ (show $ memget m (pc+1)) ++ " with accumulator %ra"]
                                    _darithmetic (Run (pc,r,m,st)) (*) (memget m (pc+1))
                                0x2 -> do
                                    tell [" multiplying value " ++ (show $ memget m (memget m (pc+1))) ++ " from address " ++ (d_memshow $ memget m (pc+1)) ++ " with accumulator %ra"]
                                    _darithmetic (Run (pc,r,m,st)) (*) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_mul: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf
dc_div cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> do
                                    tell [" dividing accumulator %ra by value " ++ (show $ memget m (pc+1))]
                                    _darithmetic (Run (pc,r,m,st)) (div) (memget m (pc+1))
                                0x2 -> do
                                    tell [" dividing accumulator %ra by value " ++ (show $ memget m (memget m (pc+1))) ++ " from address " ++ (d_memshow $ memget m (pc+1))]
                                    _darithmetic (Run (pc,r,m,st)) (div) (memget m (memget m (pc+1)))
                                inv -> error $ "Interpreter: c_div: invalid flag " ++ (show inv)
    where mask8 = cmd `shiftR` 8 .&. 0xf

dc_ret cmd (Run (pc,r,m,st)) = do
    tell [" returning to address 0x" ++ (showHex jto "") ++ ", recursion depth is now " ++ (show $ (length st) - 1)]
    return (Run (jto,r,m,pst))
    where (pst,jto) = stapop st

dc_call cmd (Run (pc,r,m,st)) = do
    tell [" calling address 0x" ++ (showHex (memget m (pc+1)) "") ++ ", recursion depth is now " ++ (show $ (length st) + 1)]
    return (Run (memget m (pc+1),r,m,stapush st (pc+2)))


d_call cmd = case mask of
                 0x00 -> dc_hlt cmd
                 0x01 -> dc_mov cmd
                 0x02 -> dc_str cmd
                 0x03 -> dc_lda cmd
                 0x04 -> dc_jmp cmd
                 0x05 -> dc_jpz cmd
                 0x06 -> dc_jnz cmd
                 0x07 -> dc_jpg cmd
                 0x08 -> dc_jpl cmd
                 0x09 -> dc_add cmd
                 0x0a -> dc_sub cmd
                 0x0b -> dc_mul cmd
                 0x0c -> dc_div cmd
                 0x0d -> dc_ret cmd
                 0x0e -> dc_call cmd
                 inv -> error $ "Interpreter: call: received invalid call code " ++ (show inv)
    where mask = cmd .&. 0xff
