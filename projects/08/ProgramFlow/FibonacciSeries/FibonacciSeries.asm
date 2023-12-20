// 	push argument 1         // sets THAT, the base address of the
@1
D=A
@ARG
A=M+D
D=M
@SP
AM=M+1
A=A-1
M=D
// 	pop pointer 1           // that segment, to argument[1]
@SP
AM=M-1
D=M
@THAT
M=D
// 	push constant 0         // sets the series' first and second
@0
D=A
@SP
AM=M+1
A=A-1
M=D
// 	pop that 0              // elements to 0 and 1, respectively       
@SP
AM=M-1
D=M
@THAT
A=M
M=D
// 	push constant 1   
@1
D=A
@SP
AM=M+1
A=A-1
M=D
// 	pop that 1              
@1
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
// 	push argument 0         // sets n, the number of remaining elements
@ARG
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	push constant 2         // to be computed to argument[0] minus 2,
@2
D=A
@SP
AM=M+1
A=A-1
M=D
// 	sub                     // since 2 elements were already computed.
@SP
AM=M-1
D=M
A=A-1
M=M-D
// 	pop argument 0          
@SP
AM=M-1
D=M
@ARG
A=M
M=D
// label LOOP
($LOOP)
// 	push argument 0
@ARG
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	if-goto COMPUTE_ELEMENT // if n > 0, goto COMPUTE_ELEMENT
@SP
AM=M-1
D=M
@$COMPUTE_ELEMENT
D;JNE
// 	goto END                // otherwise, goto END
@$END
0;JMP
// label COMPUTE_ELEMENT
($COMPUTE_ELEMENT)
// 	push that 0
@THAT
A=M
D=M
@SP
AM=M+1
A=A-1
M=D
// 	push that 1
@1
D=A
@THAT
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
// 	pop that 2
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
// 	push pointer 1
@THAT
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
// 	add
@SP
AM=M-1
D=M
A=A-1
M=M+D
// 	pop pointer 1 
@SP
AM=M-1
D=M
@THAT
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
// 	pop argument 0          
@SP
AM=M-1
D=M
@ARG
A=M
M=D
// 	goto LOOP
@$LOOP
0;JMP
// label END
($END)
// execution complete, loop forever
(VM_TRANSLATOR_END)
@VM_TRANSLATOR_END
0;JMP
