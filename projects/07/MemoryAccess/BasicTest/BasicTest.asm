// push constant 10
@10
D=A
@SP
AM=M+1
A=A-1
M=D
// pop local 0
@SP
AM=M-1
D=M
@LCL
A=M
M=D
// push constant 21
@21
D=A
@SP
AM=M+1
A=A-1
M=D
// push constant 22
@22
D=A
@SP
AM=M+1
A=A-1
M=D
// pop argument 2
@2
D=A
@ARG
D=D+M
@R13
M=D
@SP
@SP
AM=M-1
D=M
@R13
A=M
M=D
// pop argument 1
@1
D=A
@ARG
D=D+M
@R13
M=D
@SP
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 36
@36
D=A
@SP
AM=M+1
A=A-1
M=D
// pop this 6
@6
D=A
@THIS
D=D+M
@R13
M=D
@SP
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 42
@42
D=A
@SP
AM=M+1
A=A-1
M=D
// push constant 45
@45
D=A
@SP
AM=M+1
A=A-1
M=D
// pop that 5
@5
D=A
@THAT
D=D+M
@R13
M=D
@SP
@SP
AM=M-1
D=M
@R13
A=M
M=D
// pop that 2
@2
D=A
@THAT
D=D+M
@R13
M=D
@SP
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 510
@510
D=A
@SP
AM=M+1
A=A-1
M=D
// pop temp 6
@SP
AM=M-1
D=M
@R11
M=D
// push local 0
@LCL
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// push that 5
@5
D=A
@THAT
A=M+D
D=M
@SP
AM=M+1
A=A-1
M=D
// add
@SP
AM=M-1
D=M
A=A-1
M=M+D
// push argument 1
@1
D=A
@ARG
A=M+D
D=M
@SP
AM=M+1
A=A-1
M=D
// sub
@SP
AM=M-1
D=M
A=A-1
M=M-D
// push this 6
@6
D=A
@THIS
A=M+D
D=M
@SP
AM=M+1
A=A-1
M=D
// push this 6
@6
D=A
@THIS
A=M+D
D=M
@SP
AM=M+1
A=A-1
M=D
// add
@SP
AM=M-1
D=M
A=A-1
M=M+D
// sub
@SP
AM=M-1
D=M
A=A-1
M=M-D
// push temp 6
@R11
D=M
@SP
AM=M+1
A=A-1
M=D
// add
@SP
AM=M-1
D=M
A=A-1
M=M+D
// execution complete, loop forever
(VM_TRANSLATOR_END)
@VM_TRANSLATOR_END
0;JMP
