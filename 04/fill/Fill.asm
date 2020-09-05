// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
(LOOP)
    @input
    M=0
    @SCREEN
    D=A
    @addr
    M=D
    @KBD
    D=M
    @FILL
    D;JGT
    @DRAW_LOOP
    D;JEQ
(FILL)
    @0
    D=A-1
    @input
    M=D
(DRAW_LOOP)
    @addr
    D=M
    @24576 // 32 * 256 + 16384
    D=D-A
    @LOOP
    D;JGE
    @input
    D=M
    @addr
    A=M
    M=D
    @addr
    M=M+1
    @DRAW_LOOP
    0;JMP
