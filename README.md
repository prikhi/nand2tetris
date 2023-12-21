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

Project 7 & 8 is a VM Translator that converts our VM's intermediate code into
assembly code for the Hack hardware. It is located in the `VMTranslator.hs`
file.

It takes an optional folder or `.vm` file as input. After translation to
assembly code, an `.asm` file will be written either inside the specified
folder or alongside a specified VM file.

The final translator handles all VM commands for the hack platform. It will
assemble a single file or assemble & concatenate all VM files if passed a
folder. It includes the bootstrapping code, so it will no longer work with the
Project 7 test files or the early Project 8 files. There are commits for each
working older version.

This uses slightly more than just the standard `base` package, however all
packages used are part of the Core Libraries, so we can still get away with
using just GHC:

```sh
runghc --ghc-arg='-Wall' VMTranslator.hs projects/08/FunctionCalls/FibonacciElement
```

You can compile it into an exectuable as well, though the number of modules has
increased:

```sh
ghc -Wcompat -Wall -Werror -O2 -RTS -threaded -odir dist -hidir dist \
    Assembler/* VMTranslator/* VMTranslator.hs -main-is VMTranslator -o vm-translator
./vm-translator projects/08/FunctionCalls/NestedCall
```


## LICENSE

GPL-3.0+


[ghcup]: https://www.haskell.org/ghcup/
[stack]: https://www.haskellstack.org/
