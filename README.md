# nand2tetris

This is me working through the various projects in the nand2tetris book.

Projects 1-5 are HDL specifications for Logic Gates, ALU, Memory, CPU, & the
general Computer. These are located in their respective folders within the
`projects/` directory.

The software projects are built using Haskell. You need [GHC][ghcup] for the
Assembler, VM Translator, & Compiler.


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

Project 10 begins the process of creating a Jack compiler that outputs
intermediate VM code. It is located in the `Compiler.hs` file.

Like the VM Translator, the compiler can operate on a single `.jack` file or a
directory. It does not concatenate the files, but outputs a matching
`<filename>.vm` file alongside each of it's input files.

We do not current compile the Jack code into VM code. Instead, we tokenize the
file, parse the token stream into a Jack abstract syntax tree and output the
final parsing result as a `<filename>.xml` file.

Project 11 will add support for actually compiling the result of parsing.

We've expanded our library set further, opting to use `parsec` for both the
lexer & parser as it can handle both `Text` input and our custom `Token` input.
As of GHC 9.6, this is also bundled with the compiler, so `runghc` should
continue to work fine:

```sh
runghc --ghc-arg='-Wall' Compiler.hs projects/10/Square
```

You can compile this into an executable as well which drops the execution time
from ~200ms to ~10ms, but is not really necessary:

```sh
ghc -Wcompat -Wall -Werror -O2 -RTS -threaded -odir dist -hidir dist \
    Compiler/* Compiler.hs -main-is Compiler -o compiler
./compiler projects/10/Square
```


## LICENSE

GPL-3.0+


[ghcup]: https://www.haskell.org/ghcup/
