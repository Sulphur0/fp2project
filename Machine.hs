module Machine where

import Data.Bits
import Compiler

type Reg = (Int,Int,Int,Int)
type Mem = [Int]
type PC  = Int
type Sta = [Int]
data Machine = Run (PC,Reg,Mem,Sta) | Halt (PC,Reg,Mem,Sta) deriving Show
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

c_mov cmd (Run (pc,r,m,st)) = case mask12 of
                                0x1 -> case mask8 of
                                         0x1 -> (Run (pc+3,regset r set $ regget r get,m,st))
                                         0x2 -> (Run (pc+3,r,memset m set $ regget r get,st))
                                0x2 -> case mask8 of
                                         0x1 -> (Run (pc+3,regset r set $ memget m get,m,st))

    where mask8  = cmd `shiftR` 8 .&. 0xf
          mask12 = cmd `shiftR` 12 .&. 0xf
          get    = (memget m (pc+1))    -- source address
          set    = (memget m (pc+2))    -- destination address

c_lda cmd (Run (pc,r,m,st)) = case mask8 of
                                        0x0 -> (Run (pc+2,regset r 0   get,m,st))
                                        0x1 -> (Run (pc+2,regset r 0 $ regget r get,m,st))
                                        0x2 -> (Run (pc+2,regset r 0 $ memget m get,m,st))

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

    where mask8 = cmd `shiftR` 8 .&. 0xf
c_sub cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> _arithmetic (Run (pc,r,m,st)) (-) (memget m (pc+1))
                                0x2 -> _arithmetic (Run (pc,r,m,st)) (-) (memget m (memget m (pc+1)))
    where mask8 = cmd `shiftR` 8 .&. 0xf
c_mul cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> _arithmetic (Run (pc,r,m,st)) (*) (memget m (pc+1))
                                0x2 -> _arithmetic (Run (pc,r,m,st)) (*) (memget m (memget m (pc+1)))
    where mask8 = cmd `shiftR` 8 .&. 0xf
c_div cmd (Run (pc,r,m,st)) = case mask8 of
                                0x0 -> _arithmetic (Run (pc,r,m,st)) (div) (memget m (pc+1))
                                0x2 -> _arithmetic (Run (pc,r,m,st)) (div) (memget m (memget m (pc+1)))
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
                 _ -> error $ "Interpreter: call: received invalid call code " ++ (show mask)
    where mask = cmd .&. 0xff
             


execute :: Machine -> Machine
execute (Halt x) = Halt x
execute (Run (pc,r,m,st)) = execute $ call (m !! pc) (Run (pc,r,m,st))

loadCode :: Machine -> Final -> Machine
loadCode (Run (pc,r,m,st)) code = if length code > maxmem then error $ "Interpreter: load code: code is too long, " ++ (show $ length code) ++ "/" ++ (show maxmem)
                                                             else (Run (pc,r,code ++ drop (length code) m, st))

printLoadedFile filename = do
    content <- readFile filename
    let memoryImage = loadCode envinit (compile $ content)
    return memoryImage

executeFile filename = do
    content <- readFile filename
    let halted = execute $ loadCode envinit (compile $ content)
    return halted
