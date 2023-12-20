// 	push constant 0    
@0
D=A
@SP
AM=M+1
A=A-1
M=D
// 	pop local 0         // sum = 0
@SP
AM=M-1
D=M
@LCL
A=M
M=D
// label LOOP
(BasicLoop.TODO$LOOP)
// 	push argument 0     
@ARG
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	push local 0
@LCL
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
// 	pop local 0	        // sum = sum + n
@SP
AM=M-1
D=M
@LCL
A=M
M=D
// 	push argument 0
@ARG
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	push constant 1
@1
D=A
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
// 	pop argument 0      // n--
@SP
AM=M-1
D=M
@ARG
A=M
M=D
// 	push argument 0
@ARG
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	if-goto LOOP        // if n > 0, goto LOOP
@SP
AM=M-1
D=M
@BasicLoop.TODO$LOOP
D;JNE
// 	push local 0        // else, pushes sum to the stack's top
@LCL
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// execution complete, loop forever
(VM_TRANSLATOR_END)
@VM_TRANSLATOR_END
0;JMP
