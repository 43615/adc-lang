# Command line arguments

Always make sure that arguments containing shell-reserved characters are escaped properly.

## Instructions
- `-i|--inter <prompt>?` starts an interactive (REPL) session. A custom prompt may be optionally specified, the default is `> `.
  - If no mode is specified, this is implied.
- `-x|--exec <macro>+` executes the argument directly as a sequence of ADC commands. Multiple macros can be given at once.
- `-f|--file <path>+` executes the specified file as a script if it exists and is readable. Multiple filenames can be given at once.
  - If non-flag arguments are given without specifying a mode (like `$ adc somefile`), this is implied.
- These can be chained together, which will execute them sequentially. `-i` has to be exited manually to continue the sequence.

## Modes
- `-r|--rest`: Restricted mode, blocks all in-language commands that interact with the OS as a basic protection against untrusted input.
- `-d|--debug`: Debug mode, logs every executed command to stderr.
- `-q|--quiet`: Quiet mode, suppresses all errors (disables stderr).

## State management
The whole interpreter state can be stored as a file. There are in-language commands for this, but it's also possible with the following flags:
- `-s|--save <path>` saves the state to a file upon exiting (normally).
- `-l|--load <path>` loads a previously saved state before beginning execution.
- `-o|--open <path>` opens a saved state, combining `-s` and `-l`.

## Other
- `-h|--help` explains the available arguments and links to language documentation.
- `-v|--version` prints the interpreter version.


# Stacks

Stacks are the core data structure that the language revolves around. The term "stack" is a metaphor for the ways in which it can be manipulated: New elements can only be added at the top, and the top one is usually removed first. More formally, stacks are also called "last in, first out" (LIFO) queues.

Adding elements on top is called *pushing* to the stack; taking elements off is called *popping* from the stack. These are the most basic operations, and as such are always *implied* instead of having to be written out.

The current amount of elements on the stack is its *depth*. More advanced operations include *reversing* or *rotating* the order of some or all elements as well as *picking* or *duplicating* one or more elements.

Most commands operate on the *main stack*. This naturally creates a postfix or RPN order of operations.

In addition, there are also named stacks called *registers*. More on them in their own section.


# Types and values

The basic units of data are called "values". Instead of the familiar concept of named variables, values are self-contained items that can be moved around arbitrarily. The only identifier of a value is its current location.

All values have one of the following *types*:
- Boolean: `T` or `F`
- Number (rational): `123.456`
- String (sequence of characters): `[written in brackets]`
- Array (containing other values): `([example] 1 2 3)`

In the following documentation, the abbreviations *B*, *N*, *S* and *A* are used. *X* means "any type". Compound types like *AN* (array of numbers) are also used.

This specific order is also the hierarchy in which the basic types (*B*, *N*, *S*) are considered to be composed of each other: A string is like an array of numbers (character values) and a number is like an array of booleans (bit representation). Values can be easily converted up or down this hierarchy if needed.


# Command structure and execution

The reliance on stacks for storing data naturally leads to a postfix operator order (aka RPN). Data has to be created first, commands are applied to it afterwards. In this documentation, the argument syntax of commands is annotated as follows (not real commands):
- `Na Nb a -> Nz`: Command `a` pops two numbers *a,b* and pushes a resulting number *z*.
- `b -> Nz`: `b` creates a number without taking input.
- `Sa c`: `c` eats a string and doesn't return anything.
- `dR -> Xz`: Command with register access (see [below](#registers)) which returns a value of any type.
- `Na e`/`Sa e`: Overloaded command, works differently depending on the type(s) of the input(s).

The "well-behaved" commands that only ever touch the annotated inputs and outputs are called *functions*. Other, "impure" commands have behaviors not shown by this syntax.


# Array polymorphism

Arrays are ordered, contiguous lists of objects with dynamic, arbitrary length. Due to the fact that the contained objects can themselves be arrays, their dimensionality is unlimited. In ADC, arrays are written `(with parentheses)`. Here's an example of inputting a 2-dimensional array:
```
((1 2) (3 4) (5 6))
```

Arrays can contain arbitrary combinations of object types as well as have arbitrary length and nesting:
```
(((()) T 1337) [whoa] ())
```

The crucial feature that makes ADC an array language is that functions can be applied to every element of an array without having to explicitly use iteration:
`(1 2 3) (4 5 6) *` results in `(4 10 18)`, an additional `2/` results in `(2 5 9)`.

In general, running a function on two arrays recursively performs it element-wise as long as the lengths match and the types of the elements are permitted (erroring otherwise). If a function is used on an array and a "scalar" object, it's instead applied to every element of the array.

The prefixes `.` ("for each") and `:` ("for each in second") alter this behavior by forcibly iterating through one of the arrays:

`(1 2 3) (4 5 6) .*` results in `((4 5 6) (8 10 12) (12 15 18))`. Using `:*` instead results in `((4 8 12) (5 10 15) (6 12 18))`.


# Numbers

The ADC interpreter uses the highly performant [Malachite](https://www.malachite.rs/) library, specifically the `Rational` type. All numbers are stored as a numerator-denominator pair, always forming a fully reduced fraction.

## Input and output

Numbers can be entered and printed in any base (radix) starting at 2. The format used for bases up to 36 is mostly conventional, with Latin letters (any case) extending the digits after 9.

*(The `i` and `o` commands change the input and output base, more on them under [Parameters](#parameters))*
```
> 135.79 P
135.79
> `3 P	# grave/backtick as negative sign since minus is a command
`3
> 17@6 P	# exponential notation with at sign
17000000
> `123.4567@`8 P	# all combined
`0.000001234567
> 2i 10001 p	# switch to base 2 input
17
> 1010i 5o p	# back to base 10 for input, print again in base 5
32
> 20o p 2/ P	# print in base 20, halve and print again
h
8.a
> 10o 16i 'deadbeef P	# number input containing letters must be escaped
3735928559
> 1@4 P		# exponent is always applied to the current input base
65536
```

For base 37 and above, a special `'enclosed'` format must be used. Numbers are represented by a series of digit values; the block can also contain an exponential part.
```
> 50i '2 3 5 7.11 13@1' P	# highest allowed digit is 49
12887861.26
> '-0.25' P		# minus can be used as negative sign since this is separate from commands
`0.5
> 37 P	# unescaped numbers are also accepted and interpreted as single digits
37
> '1.25@'2 p	# this leniency can be taken this far
`0.0006
> '1 0'o P  # print again in base 50
'-0.0 1 25'
```

## Arithmetic
- `Na Nb + -> Nz` adds two numbers.
- `Na Nb - -> Nz` subtracts *b* from *a*.
- `Na Nb * -> Nz` multiplies two numbers.
  - `Na Bb * -> Nz` multiplies with a boolean: `T`=1, `F`=0.
- `Na Nb / -> Nz` divides *a* by *b*.


# Strings

Strings are sequences of Unicode characters, stored in the UTF-8 format.

As in the original `dc`, strings are crucial for programming: they can be interpreted as a series of commands (called a [macro](#macros) to differentiate from data strings).

Since nesting is more or less required for programming, string input uses `[square brackets]`.


# Parameters

These are variable options which control I/O operations.


# Registers

Secondary storage locations that can be accessed in various ways.


# Macros

Strings can be executed by the `x` command, which has several overloaded modes:
- `Sa x` simply executes string *a* as a series of ADC commands.
- `Sa Nb x` executes *a* *b* times.
- `Sa Bb x` executes *a* if *b* is `T`.