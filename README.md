# FP2 project
Simple Assembly-like Language interpreter written in Haskell.

## Specifications
The simulated computer is a triplet data structure, with first element representing the program counter, second element being an array of four registers labeled `ra`, `rb`, `rc`, `rd` and last element being an 100 element array representing memory addresses indexed from 0 to 99, each cell being the size of haskell's `Int` datatype.

Valid arguments include:
- raw values, represented by `$<val>`
- memory addresses, represented by `[<adr>]`
- registers, represented by `%<reg>` - valid registers are `ra` `rb` `rc` `rd`
- line labels, represented by `.<line>` - used in jump-like instructions. For example, `.4` means the same as the address of the call located on the 4th line of code, counting from 0

You can call the following instructions:
- `hlt`
- `mov <src> <dst>` - moves values from `<src>` to `<dst>`. Valid arguments are:
  - `<src>` is register, `<dst>` is register or memory address
  - `<src>` is memory address, `<dst>` is register
- `str <val> <dst>` - stores a raw value `<val>` into `<dst>` memory address
- `lda <src>` - stores a raw value or a value in a given memory address inside the `ra` register
- `jmp <adr>` - sets program counter unconditionally to given memory address or line label
- `jmz <adr>` - sets program counter to given memory address or line label if value in register `ra` equals 0
- `jnz <adr>` - sets program counter to given memory address or line label if value in register `ra` is different from 0
- `jmg <adr>` - sets program counter to given memory address or line label if value in register `ra` is greater than 0
- `jml <adr>` - sets program counter to given memory address or line label if value in register `ra` is less than 0
- `add <arg>` - adds raw value `<arg>` or value under given memory address `<arg>` to value in register `ra`, stores the result in register `ra`
- `sub <arg>` - subtracts raw value `<arg>` or value under given memory address `<arg>` from value in register `ra`, stores the result in register `ra`
- `mul <arg>` - multiplies raw value `<arg>` or value under given memory address `<arg>` with value in register `ra`, stores the result in register `ra`
- `div <arg>` - performs whole number division of value in register `ra` by the given raw value `<arg>` or value under given memory address `<arg>`, stores the result in register `ra`


Example:
using the following code (provided in file [fib](https://github.com/Sulphur0/fp2project/blob/main/fib)) for generating the 10th fibonacci number:
```
str $0  [91]
str $1  [92]
str $10 [93]
lda [93]
jpg .6
hlt
sub $1
mov %ra [93]
lda [91]
add [92]
mov %ra %rb
mov [91] %rc
mov %rb [91]
mov %rc [92]
jmp .3
```
and running main, then inputting the filename `"fib"`, yields the following result:

`HALT (13,(0,55,34,0),[514,0,91,514,1,92,514,10,93,515,93,7,14,0,10,1,4609,0,93,515,91,521,92,4353,0,1,8449,91,2,4609,1,91,4609,2,92,4,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,55,34,0,0,0,0,0,0,0])`

that being the final state of the simulated machine. First value in the triple, `17`, is the address of the final executed instruction (also the final program counter value), second value, `(0,55,34,0)`, are the final values of simulated registers `(ra,rb,rc,rd)`, with register `rb` containing the 10th fibonacci number and `rc` its predecessor, whereas the last triplet value is the giant 100-element array is the final image of the machine's memory.
