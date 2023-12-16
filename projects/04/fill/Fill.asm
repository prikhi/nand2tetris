// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen
// by writing 'black' in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen by writing
// 'white' in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// screen = 8192
@8192
D = A
@screen_size
M = D
// pixel = 0
@pixel
M = 0


(CHECK_KEYBOARD)
// R0 = KBD
@KBD
D = M
@R0
M = D
// if D == 0 goto SET_PIXEL
@SET_PIXEL
D;JEQ
// R0 = -1
@R0
M = -1
@SET_PIXEL
0;JMP
// goto SET_PIXEL


(SET_PIXEL)
// if pixel == R0 goto DRAW
@R0
D = M
@pixel
D = D - M
@DRAW
D;JEQ
// pixel = R0
@R0
D = M
@pixel
M = D
// i = 0
@i
M = 0
// goto DRAW
@DRAW
0;JMP


(DRAW)
// if i == screen_size goto CHECK_KEYBOARD
@i
D = M
@screen_size
D = D - M
@CHECK_KEYBOARD
D;JEQ
// R1 = SCREEN + i
@i
D = M
@SCREEN
D = A + D
@R1
M = D
// *R1 = pixel
@pixel
D = M
@R1
A = M
M = D
// i = i+1
@i
M = M + 1
// goto CHECK_KEYBOARD
@CHECK_KEYBOARD
0;JMP
