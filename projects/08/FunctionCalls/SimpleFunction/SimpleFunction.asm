// function SimpleFunction.test 2
(SimpleFunction.SimpleFunction.test)
@LCL
A=M
M=0
A=A+1
M=0
@2
D=A
@SP
M=M+D
// 	push local 0
@LCL
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	push local 1
@1
D=A
@LCL
A=M+D
D=M
@SP
AM=M+1
A=A-1
M=D
// 	add
@SP
AM=M-1
D=M
A=A-1
M=M+D
// 	not
@SP
A=M-1
M=!M
// 	push argument 0
@ARG
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	add
@SP
AM=M-1
D=M
A=A-1
M=M+D
// 	push argument 1
@1
D=A
@ARG
A=M+D
D=M
@SP
AM=M+1
A=A-1
M=D
// 	sub
@SP
AM=M-1
D=M
A=A-1
M=M-D
// 	return
@LCL
D=M
@R13
M=D
@5
A=D-A
D=M
@R14
M=D
@SP
AM=M-1
D=M
@ARG
A=M
M=D
D=A+1
@SP
M=D
@R13
AM=M-1
D=M
@THAT
M=D
@R13
AM=M-1
D=M
@THIS
M=D
@R13
AM=M-1
D=M
@ARG
M=D
@R13
AM=M-1
D=M
@LCL
M=D
@R14
A=M
0;JMP
// execution complete, loop forever
(VM_TRANSLATOR_END)
@VM_TRANSLATOR_END
0;JMP
