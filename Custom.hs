module Custom where

import Data.Bits
import SCompilerCustom

type Registers a = (a,a,a,a)
type Memory a = [a]
data Computer a = ACTIVE (Int,Registers a,Memory a) | HALT (Int,Registers a,Memory a)
                deriving Show

maxmem :: Int
maxmem = 100
-- this is very customizable

envinit :: Computer Int
envinit = ACTIVE (0,(0,0,0,0),[0 | _<-[0..(maxmem - 1)]])

envhalt :: Computer Int -> Computer Int
envhalt (ACTIVE (pc,reg,mem)) = HALT (pc,reg,mem)
envhalt x = x
  
loadCode :: Computer Int -> [Int] -> Computer Int
loadCode (ACTIVE (pc,reg,mem)) code = if length code > maxmem then error $ "Interpreter: load code: code is too long, " ++ (show $ length code) ++ "/" ++ (show maxmem)
                                                              else (ACTIVE (pc,reg,code ++ drop (length code) mem))

-- memory get set
memget :: Memory Int -> Int -> Int
memget mem pointer          = if pointer > maxmem then error $ "Interpreter: get mem: pointer "++(show pointer)++" out of bounds"
                                                  else mem !! pointer

memset :: Memory Int -> Int -> Int -> Memory Int
memset mem pointer value    = if pointer > maxmem then error $ "Interpreter: set mem: pointer "++(show pointer)++" out of bounds"
                                                  else (take pointer mem) ++ [value] ++ (drop (pointer+1) mem)

-- register get set
getr :: Registers Int -> Int -> Int
getr (r,_,_,_) 0x0 = r
getr (_,r,_,_) 0x1 = r
getr (_,_,r,_) 0x2 = r
getr (_,_,_,r) 0x4 = r
getr _ inv         = error $ "Interpreter: get register: Invalid register address: " ++ (show inv)

setr :: Registers Int -> Int -> Int -> Registers Int
setr (ra,rb,rc,rd) 0x0 v = (v,rb,rc,rd)
setr (ra,rb,rc,rd) 0x1 v = (ra,v,rc,rd)
setr (ra,rb,rc,rd) 0x2 v = (ra,rb,v,rd)
setr (ra,rb,rc,rd) 0x4 v = (ra,rb,rc,v)
setr _ inv _             = error $ "Interpreter: set register: Invalid register address: " ++ (show inv)

-- master call
call :: Int -> Computer Int -> Computer Int


c_hlt :: Int -> Computer Int -> Computer Int
c_mov :: Int -> Computer Int -> Computer Int
c_lda :: Int -> Computer Int -> Computer Int
c_str :: Int -> Computer Int -> Computer Int
c_jmp :: Int -> Computer Int -> Computer Int
c_jpz :: Int -> Computer Int -> Computer Int
c_jnz :: Int -> Computer Int -> Computer Int
c_jpg :: Int -> Computer Int -> Computer Int
c_jpl :: Int -> Computer Int -> Computer Int
c_add :: Int -> Computer Int -> Computer Int
c_sub :: Int -> Computer Int -> Computer Int
c_mul :: Int -> Computer Int -> Computer Int
c_div :: Int -> Computer Int -> Computer Int

c_hlt _ (ACTIVE x) = (HALT x)
c_str cmd (ACTIVE (pc,reg,mem)) = if and [mask12 == 0x0,mask8 == 0x2] then (ACTIVE (pc+3,reg,memset mem (dst) (src)))
                                                                      else error $ "Interpreter: str: invalid argument(s)"
    where mask8  = cmd `shiftR` 8 .&. 0xF
          mask12 = cmd `shiftR` 12 .&. 0xF
          src    = memget mem (pc+1)
          dst    = memget mem (pc+2)

c_mov cmd (ACTIVE (pc,reg,mem)) = case mask12 of
                                    0x1 -> case mask8 of
                                            -- reg -> reg
                                             0x1 -> (ACTIVE (pc+3,setr reg (dst) (getr reg (src)),mem))
                                            -- reg -> mem
                                             0x2 -> (ACTIVE (pc+3,reg,memset mem (dst) (getr reg (src))))
                                             _ -> error $ "Interpreter: mov: invalid argument passed to dst"
                                    0x2 -> case mask8 of
                                             -- mem -> reg
                                             0x1 -> (ACTIVE (pc+3,setr reg (dst) (memget mem (src)),mem))
                                             _ -> error $ "Interpreter: mov: invalid argument passed to dst"
                                    _ -> error $ "Interpreter: mov: invalid argument passed to src"
    where mask8  = cmd `shiftR` 8 .&. 0xF
          mask12 = cmd `shiftR` 12 .&. 0xF
          src    = memget mem (pc+1)
          dst    = memget mem (pc+2)

c_lda cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- value
                                  0x0 -> (ACTIVE (pc+2,setr reg 0 (src), mem))
                                  -- other register
                                  0x1 -> (ACTIVE (pc+2,setr reg 0 (getr reg (src)), mem))
                                  -- memory address
                                  0x2 -> (ACTIVE (pc+2,setr reg 0 (memget mem (src)), mem))
                                  _ -> error $ "Interpreter: lda: invalid argument"
    where mask8 = cmd `shiftR` 8 .&. 0xF
          src   = memget mem (pc+1)

c_jmp cmd (ACTIVE (pc,reg,mem)) = if or [mask8==0x2,mask8==0x4] then (ACTIVE (memget mem (pc+1),reg,mem))
                                                                else error $ "Interpreter: jmp: invalid argument"
    where mask8 = cmd `shiftR` 8 .&. 0xF

c_jpz _ (ACTIVE (pc,reg,mem)) = if (getr reg 0) == 0 then
                                                  (ACTIVE (memget mem (pc+1),reg,mem))
                                                  else
                                                  (ACTIVE (pc+2,reg,mem))
c_jnz _ (ACTIVE (pc,reg,mem)) = if (getr reg 0) /= 0 then
                                                  (ACTIVE (memget mem (pc+1),reg,mem))
                                                  else
                                                  (ACTIVE (pc+2,reg,mem))
c_jpg _ (ACTIVE (pc,reg,mem)) = if (getr reg 0) > 0 then
                                                  (ACTIVE (memget mem (pc+1),reg,mem))
                                                  else
                                                  (ACTIVE (pc+2,reg,mem))
c_jpl _ (ACTIVE (pc,reg,mem)) = if (getr reg 0) < 0 then
                                                  (ACTIVE (memget mem (pc+1),reg,mem))
                                                  else
                                                  (ACTIVE (pc+2,reg,mem))
c_add cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- mem addr
                                  0x2 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 + (memget mem (src))),mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 + (src)),mem))
                                  _ -> error $ "Interpreter: add: invalid argument"
    where mask8 = cmd `shiftR` 8 .&. 0xF
          src   = memget mem (pc+1)
c_sub cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- mem addr
                                  0x2 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 - (memget mem (src))),mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 - (src)),mem))
                                  _ -> error $ "Interpreter: sub: invalid argument"
    where mask8 = cmd `shiftR` 8 .&. 0xF
          src   = memget mem (pc+1)
c_mul cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- mem addr
                                  0x2 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 * (memget mem (src))),mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 * (src)),mem))
                                  _ -> error $ "Interpreter: mul: invalid argument"
    where mask8 = cmd `shiftR` 8 .&. 0xF
          src   = memget mem (pc+1)
c_div cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- mem addr
                                  0x2 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 `div` (memget mem (src))),mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr reg 0 (getr reg 0 `div` (src)),mem))
                                  _ -> error $ "Interpreter: div: invalid argument"
    where mask8 = cmd `shiftR` 8 .&. 0xF
          src   = memget mem (pc+1)

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
                 0x0A -> c_sub cmd
                 0x0B -> c_mul cmd
                 0x0C -> c_div cmd
                 _ -> error $ "Interpreter: call: received invalid call code " ++ (show mask)
    where mask = cmd .&. 0xFF



execute :: Computer Int -> Computer Int
execute (HALT x) = HALT x
execute (ACTIVE (pc,reg,mem)) = execute $ call (mem !! pc) (ACTIVE (pc,reg,mem))

printLoadedFile filename = do
    content <- readFile filename
    let memoryImage = loadCode envinit (compileCode $ content)
    return memoryImage

executeFile filename = do
    content <- readFile filename
    let halted = execute $ loadCode envinit (compileCode $ content)
    return halted
