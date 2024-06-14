# FP2 project
Simple Assembly-like Language interpreter written in Haskell.

## Specifications
The simulated computer is a quadruplet data structure, with first element representing the program counter, second element being an array of four registers labeled `ra`, `rb`, `rc`, `rd`, third element being an 100 element array representing memory addresses indexed from 0 to 99, each cell being the size of haskell's `Int` datatype, and the last element being a program counter stack (pc stack).

Valid arguments for machine calls include:
- raw values, represented by `$<val>`
- memory addresses, represented by `[<adr>]`
- registers, represented by `%<reg>` - valid registers are `ra` `rb` `rc` `rd`
- labels, represented by `.<line>` - used in jump-like instructions. A label is created by writing it down first on an empty line, after which any jumps to that label will automatically move the program counter to the call right after the label declaration. Labels with `<line>` set to integer `n` are automatically declared to refer to the `n`-th line of non-whitespace, no-only-comment and non-label-declaring code, counting from 0.

You can call the following instructions:
- `hlt` - halts the machine
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
- `call <adr>` - jumps unconditionally to the given address or label, pushing current program counter onto pc stack
- `ret` - sets program counter to value popped from the pc stack

You can comment your code using `;`.

## Example:
using the following code (provided in file [fib](https://github.com/Sulphur0/fp2project/blob/main/fib)) for generating the 10th fibonacci number:
```asm
; load initial values
str $1  [91]		; 0th fib number
str $1  [92]		; 1st fib number
str $10 [93]		; calculate 10th fib number

.loop
lda [93]		; |
sub $1			; |
mov %ra [93]		; V decrement iterator
jpg .fib		;   if iterator is 0
hlt			;   halt
			;   else calculate next fib number
.fib
lda [92]		; load nth number
add [91]		; add to it (n-1)th number
mov %ra %rb		; store the result, then
mov [92] %rc		; |
mov %rc [91]		; |
mov %rb [92]		; V move nth number to (n-1)th
jmp .loop		;   and (n+1)th to nth
```
and running main, then inputting the filename `"fib"`, yields the following result:

```
ghci> :l Main
[1 of 3] Compiling Compiler         ( Compiler.hs, interpreted )
[2 of 3] Compiling Machine          ( Machine.hs, interpreted )
[3 of 3] Compiling Main             ( Main.hs, interpreted )
Ok, three modules loaded.
ghci> main
"fib"
Halt (18,(0,89,55,0),[514,1,91,514,1,92,514,10,93,515,93,10,1,4609,0,93,1031,19,0,515,92,521,91,4353,0,1,8449,92,2,4609,2,91,4609,1,92,1028,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,55,89,0,0,0,0,0,0,0],[])
```

that being the final state of the simulated machine. First value in the quadruplet, `18`, is the address of the final executed instruction (also the final program counter value), second value, `(0,89,55,0)`, are the final values of simulated registers `(ra,rb,rc,rd)`, with register `rc` containing the 10th fibonacci number and `rb` its successor, the third value is the giant 100-element array is the final image of the machine's memory, and the last value is an (empty) pc stack.

## More examples
calculating the sum of whole numbers from 0 to 100:
```asm
; loop 100 times
str $100 [50]
lda [60]	; this code	<-here--
add [50]	; sums numbers		|
mov %ra [60]	; from 0 to 100		|
		;			|
lda [50]	;<	this code	|
sub $1		;<	decrements	|
mov %ra [50]	;<	the iterator	|
jpg .1		; this jmp will jump to |
```

calculating n! (with jumps):
```asm
; calculate n!
str $8 [70]
str $8 [71]     ; we also want a second
                ; copy of n
jmp .facloop

.endfac
lda [71]        ; present result in register a
hlt

.facloop
lda [70]
sub $1
jpz .endfac
mov %ra [70]
mul [71]
mov %ra [71]
jmp .facloop
```

calculating n! (with call and ret):
```asm
; calculate n!
str $8 [70]
mov [70] %rc    ; rc will count down n
mov [70] %rb    ; rb will have our result
call .fac
mov %rb %ra     ; present result in register a
hlt

.fac
lda %rc
sub $1
jpz .exit
mov %ra %rc
mov %rb [80]
mul [80]
mov %ra %rb
call .fac
.exit
ret
```
