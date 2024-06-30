# Functional Programming: Racket Parser

## Overview
This project contains a simple parser written in Racket, a functional programming language. The parser checks for syntactic correctness of programs based on predefined grammar rules. It is designed to demonstrate the application of functional programming principles in developing a parser that utilizes monadic types for error handling.

## How to Run
To run the parser, follow these steps:
1. Install DrRacket from [Racket Lang Official Site](https://racket-lang.org/).
2. Open the `parse.rkt` file in DrRacket.
3. To test the parser, load one of the test files from the `test_files` directory by using the following command in the DrRacket console:
   (parse "test_files/File01.txt")
   Or, with the test files are located in the same directory as the `parse.rkt` file, navigate to the bottom of `parse.rkt` and replace the file in the parse(x) command with the desired test file.

## Features
- File Input Handling: Correctly reads and processes input files for syntax verification.
- Syntax Verification: Returns "Accept" if the input is syntactically correct or provides a detailed error message specifying the line number of the first syntax error found.
- Error Reporting: If a syntax error is detected, the offending line is printed to aid debugging.
- Functional Design: The parser is implemented using functional programming techniques, including function composition, recursion, and higher-order functions.

## Examples
Below are some examples of how to use the parser with different input files:

For a syntactically correct file:
(parse "test_files/File01.txt")
Output: Accept

For a file with a syntax error:
(parse "test_files/File03.txt")
Output: Syntax error found on line 2

## Grammar
The grammar used by this parser is detailed in the `grammar.md` file. It describes the syntactic rules that determine the structure of the valid programs this parser can evaluate.
