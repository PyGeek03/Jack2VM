# Jack2VM
Jack to VM compiler for Nand2Tetris course


## Quick start:
After cloning this repo, compile the project using:

`ghc -O JackCompiler.hs`

To compile a single `[filename].jack` (in Nand2Tetris' Jack code) to `[filename].vm` (in Nand2Tetris' VM code:

`JackCompiler [filename].jack`

To compile all `.jack` files in `[directory]` to `.vm` files:

`JackCompiler [directory]`
