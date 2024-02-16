# Toy Language Interpreter

This repository contains the implementation of a simple interpreter for a toy programming language written in OCaml. The language supports basic arithmetic and boolean expressions, variable assignments, conditional statements, and loops.

## Features

- **Arithmetic Expressions**: Supports natural numbers, addition, subtraction, multiplication, and variable references.
- **Boolean Expressions**: Includes true, false, equality checks, negation, and conjunction.
- **Commands**: Allows variable assignment, sequential execution, conditional branching, and while loops.
- **State Management**: Maintains a state as a list of variable-value pairs, supporting variable lookup and update.

## Getting Started

### Prerequisites

Ensure you have OCaml installed on your system. You can check your installation by running:

### Installation

1. Clone the repository to your local machine:

2. Navigate to the cloned directory:

3. Compile the code (if necessary):

### Usage

Run the interpreter with:

## Code Overview

### Types and Exceptions

- **Basic Types**: Includes `var`, `value`, `state`, `aexp` (arithmetic expressions), and `bexp` (boolean expressions).
- **Exceptions**: Defines a custom `Error` exception for runtime errors.

### Evaluation Functions

- **Arithmetic Expressions**: `eval_aexp` evaluates arithmetic expressions to integers.
- **Boolean Expressions**: `eval_bexp` evaluates boolean expressions to boolean values.
- **Commands**: `eval_cmd` interprets commands, altering the program state accordingly.

### Utility Functions

- **State Management**: `lookup` retrieves the value of a variable from the state.
- **Type Conversion**: `convert_to_int` and `convert_to_bool` convert values to integers and booleans, respectively.

## Examples

### Arithmetic Expression

Evaluates to 74:

### Boolean Expression

Evaluates to False:


### Command Execution

Assigns 42 to variable `x`:

## Contributing

Contributions are welcome! Please feel free to submit a pull request or open an issue for discussion.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
