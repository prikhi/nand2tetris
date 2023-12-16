// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// Assumes that R0 >= 0, R1 >= 0, and R0 * R1 < 32768.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// product = 0; i = 0
@product
M = 0
@i
M = 0

(LOOP)
// if (i == R0) goto STOP
@i
D = M
@R0
D = D - M
@STOP
D;JEQ
// product = product + R1
@R1
D = M
@product
M = D + M
// i = i + 1
@i
M = M + 1
// goto LOOP
@LOOP
0;JMP

(STOP)
// R2 = product
@product
D = M
@R2
M = D

(END)
@END
0;JMP
