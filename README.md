# fp2project
Simple Assembly-like Language interpreter written in Haskell.

## Specifications
The simulated computer is a triplet data structure, with first element representing the program counter, second element being an array of four registers labeled ```ra, rb, rc, rd``` and last element being an 100 element array representing memory addresses indexed from 0 to 99, each cell being the size of haskell's ```Int``` datatype.

You can call the following routines:
- ```hlt``` - halts the program
- ```mov <src> <dst>``` - moves values from register to register or memory and from memory to register
- ```str <dst> <val>``` - stores given value into desired memory address
- ```jmp <adr>``` - sets program counter unconditionally to given memory address
- ```jmz <adr>``` - sets program counter to given memory address if value in register ```ra``` equals 0
- ```jnz <adr>``` - sets program counter to given memory address if value in register ```ra``` is different from 0
- ```jmg <adr>``` - sets program counter to given memory address if value in register ```ra``` is greater than 0
- ```jml <adr>``` - sets program counter to given memory address if value in register ```ra``` is less than 0
- ```add <adr>``` - adds value under given memory address to register ```ra```, stores the result in register ```ra```
- ```sub <adr>``` - subtracts value under given memory address from register ```ra```, stores the result in register ```ra```
- ```mul <adr>``` - multiplies value under given memory address with register ```ra```, stores the result in register ```ra```
- ```div <adr>``` - performs whole number division of value under register ```ra``` by the value under given memory address, stores the result in register ```ra```

Valid arguments include:
- ```$<val>``` - represents whole number values
- ```[<adr>]``` - represents memory address
- ```%<reg>``` - represents register, valid registers are ```ra``` ```rb``` ```rc``` ```rd```

Example:
loading the following code for generating the 10th fibonacci number:
```
str [91] $0
str [92] $1
str [93] $1
str [94] $10
mov [94] %ra
jnz [18]
hlt
sub [93]
mov %ra [94]
mov [91] %ra
add [92]
mov %ra %rb
mov [91] %rc
mov %rb [91]
mov %rc [92]
jmp [12]
```

and executing it gives the following result:

```HALT (17,(0,55,34,0),[8194,91,0,8194,92,1,8194,93,1,8194,94,10,8449,94,0,517,18,0,521,93,4609,0,94,8449,91,0,520,92,4353,0,1,8449,91,2,4609,1,91,4609,2,92,515,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,55,34,1,0,0,0,0,0,0])```

that being the final state of the simulated machine. ```17``` is the address of the final executed instruction (also the final program counter value), ```(0,55,34,0)``` are the final values of simulated registers, with register ```rb``` containing the 10th fibonacci number and ```rc``` its predecessor, whereas the giant 100-element array is the final image of the machine's memory.
