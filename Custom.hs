module Custom where

import Data.Bits
import SCompilerCustom

type Registers a = (a,a,a,a)
type Memory a = [a]
data Computer a = ACTIVE (Int,Registers a,Memory a) | HALT (Int,Registers a,Memory a)
                deriving Show

envinit :: Computer Int
envinit = ACTIVE (0,(0,0,0,0),[0 | _<-[0..99]])

envhalt :: Computer Int -> Computer Int
envhalt (ACTIVE (pc,reg,mem)) = HALT (pc,reg,mem)
envhalt x = x

loadCode :: Computer Int -> [Int] -> Computer Int
loadCode (ACTIVE (pc,reg,mem)) code = (ACTIVE (pc,reg,code ++ drop (length code) mem))

memget :: Memory Int -> Int -> Int
memget mem pointer = mem !! pointer

memset :: Memory Int -> Int -> Int -> Memory Int
memset mem pointer value = (take pointer mem) ++ [value] ++ (drop (pointer+1) mem)

getr :: Registers Int -> Int -> Int
getr (r,_,_,_) 0 = r
getr (_,r,_,_) 1 = r
getr (_,_,r,_) 2 = r
getr (_,_,_,r) 3 = r

setr :: Int -> Registers Int -> Int -> Registers Int
setr v (ra,rb,rc,rd) 0 = (v,rb,rc,rd)
setr v (ra,rb,rc,rd) 1 = (ra,v,rc,rd)
setr v (ra,rb,rc,rd) 2 = (ra,rb,v,rd)
setr v (ra,rb,rc,rd) 3 = (ra,rb,rc,v)

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
c_str _ (ACTIVE (pc,reg,mem)) = (ACTIVE (pc+3,reg,memset mem (memget mem (pc+2)) (memget mem (pc+1))))
c_mov cmd (ACTIVE (pc,reg,mem)) = case mask12 of
                                   0x1 -> case mask8 of
                                            -- reg -> reg
                                             0x1 -> (ACTIVE (pc+3,setr (getr reg (memget mem (pc+1))) reg (memget mem (pc+2)),mem))
                                            -- reg -> mem
                                             0x2 -> (ACTIVE (pc+3,reg,memset mem (memget mem (pc+2)) (getr reg (memget mem (pc+1)))))
                                   0x2 -> case mask8 of
                                            -- mem -> reg
                                             0x1 -> (ACTIVE (pc+3,setr (memget mem (memget mem (pc+1))) reg (memget mem (pc+2)),mem))
    where mask8  = cmd `shiftR` 8 .&. 0xF
          mask12 = cmd `shiftR` 12 .&. 0xF 
c_lda cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- value
                                  0x0 -> (ACTIVE (pc+2,setr (memget mem (pc+1)) reg 0, mem))
                                  -- other register
                                  0x1 -> (ACTIVE (pc+2,setr (getr reg (memget mem (pc+1))) reg 0,mem))
                                  -- memory address
                                  0x2 -> (ACTIVE (pc+2,setr (memget mem (memget mem (pc+1))) reg 0,mem))
    where mask8 = cmd `shiftR` 8 .&. 0xF
c_jmp _ (ACTIVE (pc,reg,mem)) = (ACTIVE (memget mem (pc+1),reg,mem))
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
                                  0x2 -> (ACTIVE (pc+2,setr (getr reg 0 + (memget mem (memget mem (pc+1)))) reg 0,mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr (getr reg 0 + (memget mem (pc+1))) reg 0,mem))
    where mask8 = cmd `shiftR` 8 .&. 0xF
c_sub cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- mem addr
                                  0x2 -> (ACTIVE (pc+2,setr (getr reg 0 - (memget mem (memget mem (pc+1)))) reg 0,mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr (getr reg 0 - (memget mem (pc+1))) reg 0,mem))
    where mask8 = cmd `shiftR` 8 .&. 0xF
c_mul cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- mem addr
                                  0x2 -> (ACTIVE (pc+2,setr (getr reg 0 * (memget mem (memget mem (pc+1)))) reg 0,mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr (getr reg 0 * (memget mem (pc+1))) reg 0,mem))
    where mask8 = cmd `shiftR` 8 .&. 0xF
c_div cmd (ACTIVE (pc,reg,mem)) = case mask8 of
                                  -- mem addr
                                  0x2 -> (ACTIVE (pc+2,setr (getr reg 0 `div` (memget mem (memget mem (pc+1)))) reg 0,mem))
                                  -- raw value
                                  0x0 -> (ACTIVE (pc+2,setr (getr reg 0 `div` (memget mem (pc+1))) reg 0,mem))
    where mask8 = cmd `shiftR` 8 .&. 0xF

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
