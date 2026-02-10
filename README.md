# Recursive Turing Machine Interpreter

A low-level language implementation that executes on an extended, recursive Turing Machine model. This project is developed as part of the **Principles of Programming Languages** course (2025-2026).

## Overview
The goal is to implement a parser and an execution system for a language where multiple Turing machines can share a single tape and call each other recursively.

## Key Features (Requirements)
* **Custom Data Structures**: Implementation of a balanced Binary Search Tree (Map) for $O(\log n)$ lookup of machines and transitions.
* **Infinite Tape**: Efficient tape representation using dual-list logic.
* **Recursive Engine**: Support for machine calls using a call stack to track return states.
* **Functional Parser**: A grammar-based parser implemented using Haskell's `Either` and `Maybe` monads for robust error handling.

## Language Syntax
The interpreter handles:
- **Alphabet**: User-defined symbols.
- **Actions**: Write (`w`), Move (`g`), and Call (`c`).
- **Machines**: Defined by states, initial/halting configurations, and transition functions.

## Tech Stack
- **Language**: Haskell
- **Compiler**: GHC
- **No External Dependencies**: Built strictly using core Haskell modules.

## Usage
Compilation:
`ghc --make Main.hs -o rTM`

Execution:
`./rTM <program_file>`

## Writing Programs
The interpreter follows the specific grammar and syntax rules outlined in the project documentation. 

For detailed information on the alphabet, machine definitions, and transition functions, please refer to the [Project Specification (PDF)](./Project_2025-2026.pdf).

### Parsable Example Program
The following program defines a simple routine: it clears any immediate data and then prints "hi" on the tape. 

```text
alphabet = {h, i}

machine clear_tape = {
  states = {s, c1, c2, c3, e}
  init_state = {s}
  halting_states = {e}
  function = {
    s _ -> c1 w h;
    c1 h -> c2 g right;
    c2 i -> c1 w _;
    c2 _ -> c3 g left;
    c3 h -> e w _;
  }
}

machine print_hi = {
  states = {s, q, p, e}
  init_state = {s}
  halting_states = {e}
  function = {
    s _ -> q w h;
    q _ -> p g right;
    p _ -> e w i;
  }
}

machine start = {
  states = {s, q, e}
  init_state = {s}
  halting_states = {e}
  function = {
    s _ -> q c clear_tape;
    q _ -> e c print_hi;
  }
}