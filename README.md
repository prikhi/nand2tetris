# nand2tetris

This is me working through the various projects in the nand2tetris book.

Projects 1-5 are HDL specifications for Logic Gates, ALU, Memory, CPU, & the
general Computer. These are located in their respective folders within the
`projects/` directory.

The software projects are built using Haskell. You need [GHC][ghcup] for the
assembler, and will likely need [stack][stack] for further projects.


## Assembler

Project 6 is an Assembler to convert Hack assembly programs into machine code
for the Hack hardware platform. It is located in the `Assembler.hs` file.

The assembler takes a `.asm` file as input & spits out the generated machine
code in a `.hack` file located in the same directory as the input file.

It only uses the standard library so you only need GHC installed & can run it
with `runghc`:

```sh
runghc --ghc-arg='-Wall' Assembler.hs projects/06/pong/Pong.asm
```

You can also compile the code into an executable instead of interpreting it:

```sh
ghc -Wcompat -Wall -Werror -O2 -RTS -threaded -odir dist -hidir dist \
    Assembler/* Assembler.hs -main-is Assembler -o assembler
./assembler projects/06/pong/Pong.asm
```


## VM Translator

Project 7 is a basic VM Translator that converts our VM's intermediate code
into assembly code for the Hack hardware. It is located in the
`VMTranslator.hs` file.

The basic translator only handle arithmetic-logic commands & stack commands. It
takes a `.vm` file as input & spits out the generated assembly code in a `.asm`
file located in the same directory as the input file.

It also only uses the standard library & requires no external dependencies:

```sh
runghc --ghc-arg='-Wall' VMTranslator.hs projects/07/MemoryAccess/StaticTest/StaticTest.vm
```

You can compile it into an exectuable as well:

```sh
ghc -Wcompat -Wall -Werror -O2 -RTS -threaded -odir dist -hidir dist \
    Assembler/* VMTranslator/* VMTranslator.hs -main-is VMTranslator -o vm-translator
./vm-translator projects/07/MemoryAccess/StaticTest/StaticTest.vm
```


## LICENSE

GPL-3.0+


[ghcup]: https://www.haskell.org/ghcup/
[stack]: https://www.haskellstack.org/
