# nand2tetris

This is me working through the various projects in the nand2tetris book.

Projects 1-5 are HDL specifications for Logic Gates, ALU, Memory, CPU, & the
general Computer. These are located in their respective folders within the
`projects/` directory.

The software projects(6-11) are built using Haskell. You need [GHC][ghcup] for the
Assembler, VM Translator, & Compiler.

Project 12 is the Hack OS & is written in Jack. It is located in the
`projects/12` directory, each individual file has been copied to it's Test
sub-folder & passes the scripted/interactive tests. The full OS has been copied
to the `projects/11/Pong` folder & compiled with our compiler. It works but
paddle drawing is slightly buggy & the game in general is slow. There are
improvements that could be made in the `Screen.jack` file that would speed this
up.


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


## Compiler

Project 10 & 11 are a Jack compiler that outputs the intermediate VM code to be
used with the VM Translator. It is located in the `Compiler.hs` file.

Like the VM Translator, the compiler can operate on a single `.jack` file or a
directory. It does not concatenate the files, but outputs a series of files
alongside each of it's input files.

The final compiler tokenizes each input file, parse the token stream into a
Jack abstract syntax tree, compiles the AST into a series of VM commands, and
outputs the final result as a `<filename>.vm` file. You can run the compiled
output by loading a compiled folder into the `VMEmulator` in the `tools`
directory.

While project 10 had us emitting XML of the parsed token stream, the final
compiler only outputs VM files. However, we still generate this XML during
parsing, & it is available for printing/writing by editing the
`Compiler.runCompiler` function.

We've expanded our library set further, opting to use `parsec` for both the
lexer & parser as it can handle both `Text` input and our custom `Token` input.
As of GHC 9.6, both the `parsec` & `text` libraries are bundled with the
compiler, so `runghc` should continue to work fine:

```sh
runghc --ghc-arg='-Wall' Compiler.hs projects/11/Pong
```

You can compile this into an executable as well which drops the execution time
from ~300ms to ~10ms, but is not really necessary for the projects:

```sh
ghc -Wcompat -Wall -Werror -O2 -RTS -threaded -odir dist -hidir dist \
    Compiler/* VMTranslator/* Compiler.hs -main-is Compiler -o compiler
./compiler projects/11/Pong
```


## LICENSE

GPL-3.0+


[ghcup]: https://www.haskell.org/ghcup/
