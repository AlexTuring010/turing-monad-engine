# turing-monad-engine

A Haskell interpreter for **Recursive Turing Machines (rTM)** — an
extension of the classic Turing machine model where machines can call
other machines recursively. Built as an undergraduate
theory-of-computation project.

## What it does

Reads a `.turing` source file, parses it (with semantic checks), and
executes the program against an input tape. Supports:

- Recursive machine calls (the "monad-engine" in the name)
- Multi-machine programs with a designated start machine
- Inline `-- comments` in source files
- Two example programs: `programs/reverse.turing` and
  `programs/addition.turing`

## Architecture

| File | Responsibility |
|---|---|
| `Main.hs` | I/O, CLI args, initial tape from user |
| `Tokenizer.hs` | Lexer — tokens, comment stripping |
| `Parser.hs` | Builds the AST and runs semantic analysis (completeness, undefined references, duplicates, missing start machine) |
| `Emulator.hs` | Executes the parsed program — runs without re-checking, since the parser already validated |
| `Types.hs` | Core data types |
| `Map.hs` | Custom AVL-tree map (`O(log n)` lookup) for machines, states, and transitions |

I implemented the map as a balanced AVL tree rather than reusing
`Data.Map`, partly as an exercise. The tape is a **dual-list** structure
(reversed-left-half + right-half with the head between them), making
head moves `O(1)`.

## Example: binary addition (the interesting one)

`programs/addition.turing` adds binary numbers given input like
`1 0 1 plus 1 0 plus 1`. The strategy is recursive: replace the first
`plus` with a blank, recurse to add the remaining numbers, then fold
that result back together with the original first number.

The zip step is the most interesting machine. It pairs bits of two
numbers, padding the shorter with zeros so addition can proceed on
aligned pairs (LSB-first). It handles `_ plus 1 1`, `_ 1 1 plus _`, and
arbitrarily mismatched bit lengths.

Test cases that all pass:

| Input | Output |
|---|---|
| `plus plus plus` | `0` |
| `1 plus plus plus 1 plus` | `10` |
| `0 plus 0 plus 0 plus 0` | `0` |
| `1 0 1 plus 1 0 plus 1 plus` | `1000` |
| `plus 1 plus 0 plus 1 plus plus` | `10` |

## Build and run

```bash
ghc --make Main.hs -o rTM -outputdir build
./rTM programs/reverse.turing
./rTM programs/addition.turing
```

Requires GHC (any recent version).

## License

MIT — see [LICENSE](./LICENSE).
