# SANTA

Santa is a simple procedural programming language created with Haskell. 

## FEATURES

- Variable (re)declaration (Local and shared memories)
- Simple arithmetic/logic/comparison operations
- Control flow with if/else, where statement
- Simple concurrency and synchronization with lock
- Others: Debugging for variable, program termination, ...

More instructions can be found inside the files.

## PREREQUISITES

To run the program, please set up the following software/environment
- Glasgow Haskell Compiler
- Haskell libraries: Data, Test.HUnit, ParSec, Sprockell

To run the files, start with the current directory `implementation` and go to the Glasgow Haskell Compiler environment
with `ghci`

## FILE STRUCTURES
- implementation/
    - tests/
        - errors/                                                       -- * Containing all error programs that raise error when compile
        - example/                                                      -- * Containing example program. See section below
        - parser/                                                       -- Tests for parsers
            - prog/                                                     -- Containing some simple test program for parsers
            - unit/                                                     -- Unit tests for parsers
        - type_scope/                                                   -- Tests for type & scope checking
            - prog/                                                     -- Containing example program for type scope checking
        - unit/                                                         -- Unit tests for type/scope checking
    - BParser.hs                                                        -- Containing parsers for the program
    - CodeGen.hs                                                        -- Containing functions to generate SprIL instructions
    - LangDef.hs                                                        -- Containing data classes used for syntax tree
    - Main.hs                                                           -- * A simple file to test the program on simple_prog.txt. Compile with `:l Main` and call `main`
    - PComb.hs                                                          -- Containing language definition and base parsers with Parser
    - simple_prog.txt                                                   -- Editable Santa file that can be run with Main.hs
    - TypeScope.hs                                                      -- Containing functions to check for type/scope
- language_definition/                                                  -- * See to understand more about the language

* are important files/sections to see

To see the language definition, go to language_definition/language_definition

To run the program, see instructions in Main.hs or tests/example/ folder

To run the unit tests in test/ folder, see instruction inside each file to compile the Haskell file and call `run`
For example: `:l tests/parser/unit/BoolOp` and `run`

To run the program test in test/ folder, see instruction inside prog/fileName.hs to compile and call `run`
For example: `:l tests/parser/prog/Program` and `run0`

## EXAMPLES
There are a number of demo programs showing various features in file tests/examples/Run.hs . To run this file,use
':l tests/examples/Run' and call runProg <fileName> with the desired program. See more description on that file

banking.txt - A simple banking system with concurrency that make multiple parallel transactions at the same time
fib.txt - Fibonacci nth number algorithm
gcd.txt - Greatest common divisor algorithm
first_order_equation.txt - A simple program that find solution for first order equation
peterson.txt - Peterson algorithm

## AUTHORS AND ACKNOWLEDGEMENTS

Le Tran Anh Duc - Group 15 - Programming Paradigm 2020 - University of Twene - Student number s2086948. For more information or suggestions for improvement, email to letrananhduc@student.utwente.nl
