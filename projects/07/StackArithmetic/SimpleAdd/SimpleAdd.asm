// push constant 7
@7
D=A
@SP
M=M+1
A=M
A=A-1
M=D
// push constant 8
@8
D=A
@SP
M=M+1
A=M
A=A-1
M=D
// add
@SP
M=M-1
A=M
D=M
A=A-1
M=M+D
// execution complete, loop forever
(VM_TRANSLATOR_END)
@VM_TRANSLATOR_END
0;JMP
