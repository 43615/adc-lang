# HEAVILY WIP - NOWHERE NEAR A LANGUAGE SPEC YET

---

# Library and executable structure

This Rust crate consists of a reference ADC language interpreter, as well as a default terminal-based wrapper executable (Cargo binary target `adc`). Run `adc --help` to see the documentation of its command line syntax.

There is some behavior specific to this included CLI wrapper, which may not be present when using a different one. If you are implementing a wrapper, you should give it equivalent behavior whenever possible. In the following documentation, such behavior is annotated with "**CLI:**".

For this implementation, the term "interpreter invocation" refers to one call of the `exec` function. This is the scope limit for any temporary options that aren't part of the `State`. **CLI:** Interpreter instances are invoked by `-i`, `-e`, and `-f`, where `-i` does it repeatedly until exited.


# Core principles

ADC can be thought of as a kind of machine code for an exotic computer architecture centered around stack memory.

The term "stack" is a metaphor for the ways in which it can be manipulated: New elements can only be added at the top ("push"), and the top one is removed first ("pop"). More formally, stacks are also called "last in, first out" (LIFO) queues.

The current amount of elements on the stack is its "depth". More advanced operations include "rotating" the order of some contents, as well as duplicating/"picking" one or more elements.

In the ADC virtual machine, most instructions operate on the "main stack". This naturally creates a postfix or RPN (reverse Polish notation) order of operations. In addition, there are secondary stacks called "registers", which are essential for any complex operation.


# Types and values

The basic units of data are called "values". Instead of the familiar concept of named variables, values are self-contained items that can be moved around arbitrarily. The only identifier of a value is its current location.

All values have one of the following types:
- Boolean: sequence of `T` (true) and `F` (false) bits
- Number: rational, theoretically unbounded
- String: sequence of characters, stored in UTF-8
- Array: continuous list of values of any type, including other arrays

Yes, this is not the usual meaning of "boolean". These are actually bit vectors of arbitrary length, which was chosen to align with the other types. You may call them "bitvecs" if it helps you sleep.


## Type annotations and restricted types

In the following documentation, types expected by commands are annotated in a compact format. Some commands can only use/return values from a subset of the type, which are annotated as follows:
- `B`: Boolean
  - `BB`: Exactly one bit
- `N`: Number
  - `NN`: Natural
  - `NZ`: Integer
  - `NC`: Unicode character ("scalar") value, any natural up to 1114111 (0x10FFFF) but excluding 55296 - 57343 (0xD800 - 0xDFFF)
  - `NP`: Pointer-sized natural, 32 or 64 bits depending on system architecture (0 - 2^N-1)
  - `NF`: Number is converted to/from floating-point (IEEE 754 64-bit) for calculation, see [below](#numbers)
- `S`: String
  - `SC`: Exactly one character
  - `SM`: Macro, a valid sequence of ADC commands (not verified before execution, behavior is undecidable due to depending on prior state)


# Command structure and execution

The reliance on stacks for storing data naturally leads to a postfix operator order (aka RPN). Data has to be put on the stack first, commands are applied to it afterwards. In this documentation, the argument syntax of commands is annotated as follows (not real commands):
- `Na Nb a -> Nz`: Command `a` pops two numbers *a,b* and pushes a resulting number *z*.
- `b -> Nz`: `b` creates a number without taking input.
- `Sa c`: `c` eats a string and doesn't return anything.
- `d⒭ -> Xz`: Command with register access (see [below](#registers)) which returns a value of any type.
- `Na e`/`Sa e`: Overloaded command, works differently depending on the type(s) of the input(s).

The "pure" commands that only ever touch the annotated inputs and outputs are called "arithmetic functions". Other commands behave in ways not shown by this syntax.

Arithmetic functions can be monadic, dyadic, or triadic (take 1, 2, or 3 arguments from the stack) and return one resulting value (which may be an array).

Commands with register access take the next character as the register number, unless the register pointer is set.

There are advanced/rarely used commands that don't fit the usual single character rule, instead being words that start with `_` (underscore) and run until the next whitespace character.

Some commands have an alternative mode, which can be enabled with `` ` `` (also used for [negative numbers](#number-io)). It only applies to the immediately following command, forming a digraph.


# Array polymorphism

Arrays are ordered, contiguous lists of objects with arbitrary length. Due to the fact that the contained objects can themselves be arrays, their dimensionality is unlimited. In ADC, arrays are written `(with parentheses)`. Here's an example of inputting a 2-dimensional array:
```
((1 2) (3 4) (5 6))
```

Arrays can contain arbitrary combinations of object types as well as have arbitrary length and nesting:
```
(((()) T 1337) [Sample Text] ())
```

The crucial feature that makes ADC an array language is that functions can be applied to every element of an array without having to explicitly use iteration:
`(1 2 3) (4 5 6) *` results in `(4 10 18)`, an additional `2/` results in `(2 5 9)`.

This implicit application of operations is limited to the pure arithmetic functions (although composition by concatenation is equivalent). The traversal procedure is as follows:
- For monadics, the function is simply applied to every element, preserving their locations in nested arrays.
- For dyadics and triadics, the resulting nested layout will have the highest dimension of all arguments:
  - Traverse array(s), for each element:
  - If the 2 or 3 arguments at the current position are only scalars, compute the function.
  - If at least one is an array, promote any scalars to arrays of correct length and recurse. Examples:
    - `(1 2 3) 4 +` and `(1 2 3) (4 4 4) +` are equivalent (`(5 6 7)`).
    - `(1 (2 3 4)) ((5 6 7) 8) +` and `((1 1 1) (2 3 4)) ((5 6 7) (8 8 8)) +` are equivalent (`((6 7 8) (10 11 12))`).
  - If there are multiple arrays of unequal length, abort the process.
- In this implementation, the "recursion" is implemented using an iterative algorithm in heap memory and the "promotion" does not actually allocate a new array. This avoids stack overflow issues on real systems and utilizes a practical minimum amount of memory (tested with recursion depths in the millions). Other interpreter implementations should employ similar algorithms if possible.


# Numbers

This ADC interpreter uses the highly performant [Malachite](https://www.malachite.rs/) library, specifically the `Rational` type (with 32-bit limbs). All numbers are stored as a (fully reduced) fraction of arbitrary-length integers. This means that numbers may hold arbitrary values, with memory usage proportional to the amount of digits. Another natural feature of using rationals is that recurring (periodic) fractional digits are supported inherently. In fact, for any natural base >=2, the set of all non-redundant integer+fractional+recurring digit sequences is bijective to the rationals.

Some of the available mathematical functions are irrational-valued, which necessitates usage of floating-point arithmetic to achieve practical performance on real computers. This places hard limits on the range and precision of the arguments and results of these functions. As mentioned [above](#type-annotations-and-restricted-types), such numbers are annotated as `NF`. This interpreter uses 64-bit ("double precision") numbers as defined by IEEE 754 "binary64", which are natively supported by all CPUs worth using (citation needed).

Some functions also have "integer" variants that aren't bound by these limits, but place further restrictions on the arguments. They are automatically selected instead of the floating-point variant whenever possible, as such results are always preferable.


## Number I/O

Numbers can be entered and printed in any base (radix) starting at 2. The format used for bases up to 36 is mostly conventional, with Latin letters (either case) extending the digits after 9. Bases are configured as [parameters](#parameters).

Format specifics:
- Since `-` (minus/hyphen) is an arithmetic function, the character `` ` `` (grave/backtick) is used as the negative sign.
- Fractional digits start with `.` (period). For values |x|<1, the leading 0 may be omitted. Recurring digits begin with the same `` ` `` character (at any point after the `.`) and continue until the number ends. For example, 1/700 can be entered as `` 0.00`142857 ``.
- Scientific/exponential notation uses `@` (at sign) instead of the more conventional e/E. The exponent values themselves are always interpreted as decimal, but applied to the current input base. Negative exponents also use `` ` ``.
- Bases above 10 use letters for digits after 9. To include letters in a number, it must be prefixed by `'` (apostrophe). Example with input base 16: `'dEaD.bEeF` = 57005.7458343505859375.
- Bases above 36 use a special `'enclosed'` format. Numbers are represented as a space-separated series of digits, which are in decimal themselves. Negatives, fractionals and exponents work identically. Example with input base 100: `` '`12 3.45 0 67@`8' `` = -1203.450067 * 100^-8.
  - Plain numbers without apostrophes are also accepted, and interpreted as decimal.

Number output is controlled by the [parameters](#parameters) K and O. Additionally, there are 3 display formats:
- Normal: `123456.7`
- Scientific: `1.234567@5`
- Fraction: `1234567 10/`

These are equally correct ways of expressing a number, as inputting them back would give the same value. By default, outputting a number generates all 3 formats and picks the shortest one (preference order as listed), but one of these may be forced using an additional parameter.


## Parameters

These are options for controlling number I/O operations:
- `NPa k` sets the output precision. This limits the amount of displayed significant digits, with 0 meaning unlimited.
  - Normal: Counts all significant digits until reaching K. Trailing digits that don't fit are removed: integer digits are replaced with 0s, fractional digits are discarded, recurring digits are only displayed as such if the whole recurring portion can fit. Rounds the remaining digits to nearest, ties to even ([avoiding biases](https://en.wikipedia.org/wiki/Rounding#Rounding_half_to_even)).
  - Scientific: Same rules, the exponent is not included in the digit count.
  - Fraction: Finds the best approximation with at most K digits in the numerator or denominator, whichever is greater.
- `K -> NPz` returns the current output precision.
- `NNa i` sets the input base, must be at least 2. Bases 11-36 require an `'apostrophe` to include letters. Bases over 36 require the `'enclosed'` format, interpreted as decimal otherwise.
- `I -> NNz` returns the current input base.
- `NNa o` sets the output base, must be at least 2. Bases over 10 are always displayed with the `'apostrophe` prefix or in the `'enclosed'` format.
- `O -> NNz` returns the current output base.
- `_nnorm`, `_nsci`, `_nfrac` force the different number formats. `_nauto` picks the shortest one.

The parameters are stored in bundles like (K, I, O, format), called a "context". These contexts are stored on their own stack, and any relevant operations always use the top context. Curly brackets are used to control contexts in a visually clear way:
- `{` pushes a new context with the defaults (0, 10, 10, auto).
- `}` pops the current context, creating a default one if there is none to return to.
- `_clctx` clears all contexts, creating a default one.


## Arithmetic

- `Na Nb + -> Nz` adds two numbers.
- `Na Nb - -> Nz` subtracts *b* from *a*.
- `Na Nb * -> Nz` multiplies two numbers.
- `Na Nb / -> Nz` divides *a* by *b*.
- TODO


# Strings

Strings are sequences of Unicode characters, stored in the UTF-8 format.

As in the original `dc`, strings enable programming since they can be executed as a series of commands (called a [macro](#macros) to differentiate from data strings).

Since nesting is required for any complex program, string input uses `[square brackets]` instead of quotes. Certain special characters may be inserted using familiar escape sequences:
- `\a`: Bell (07)
- `\b`: Backspace (08)
- `\t`: Horizontal tab (09)
- `\n`: Line feed (0A)
- `\v`: Vertical tab (0B)
- `\f`: Form feed (0C)
- `\r`: Carriage return (0D)
- `\e`: Escape (1B)
- `\\`: Backslash itself (5C)
- `\[` and `\]`: Unpaired square brackets (5B, 5D), not processed as string delimiters. These need to be escaped multiple (2^N-1) times in nested strings, as one would do with quotes in other languages. Example: Executing `[[[foo\\\\\\\]bar]]]` parses it into `[[foo\\\]bar]]`, then `[foo\]bar]` and finally `foo]bar` (as it would be printed).
  - This can be avoided using [type conversion](#type-conversion): Instead of `[foo\]bar]`, the desired string can be assembled like `[foo]93 <TODO> +[bar]+`, which will be preserved through nested parsing and only executed at the bottom level.
- `\XX`: Byte literal with exactly two hexadecimal digits (uppercase). Must form a valid UTF-8 sequence, string is invalid otherwise.


# Stack operations

TODO


# Array operations and indexing

TODO


# Registers

Secondary storage locations that can be accessed in various ways.


# Type conversion

TODO


# Macros

Strings can be executed with the `x` command, which has several overloaded modes:
- `SMa x` simply executes string *a* as a series of ADC commands.
- `SMa NNb x` executes *a* *b* times.
- `SMa Bb x` executes *a* once for every `T` bit in *b*.
- If the arguments are arrays, their contents are executed analogously, first element first. The array is effectively reversed and appended to the call stack, which also necessarily flattens nested arrays. *a* must only contain strings, *b* (if used) can't contain any strings. For the dyadic form, the array lengths and nesting topologies must match exactly (as [usual](#array-polymorphism)).


# Other commands

- `p` prints the top-of-stack value, leaving it unchanged.
- `Xa P` pops and prints the top-of-stack.
- `` `p ``/`` `P `` prints without a newline.
- `fp`/`fP` prints the whole stack line-by-line, keeping/removing the contents. The top value is printed first, prefix `` ` `` to reverse.
- `"` toggles string mode for these printing commands. Output is written to a string buffer instead of stdout, the closing `"` pushes the string to the stack.
- `Xa z -> NZz` converts a value into its type discriminant:
  - Scalars: Boolean is `` `1 ``, number is `` `2 ``, string is `` `3 ``.
  - Arrays: Length of the array.
- `fz -> NNz` returns the current depth of the stack (also considered an array).
- `(Xa) Z -> (NZz)` converts an entire array to discriminants, keeping its nesting layout.
- `q` quits the ADC interpreter. If the [register pointer](#registers) is set, the lowest byte of its integer part is returned as the exit code.
  - `` `q `` is a "hard" quit, which may be handled differently.
  - **CLI:** Soft quit ends the current `-i`/`-e`/`-f` invocation, the exit code is updated by each `q` and returned at the very end. Hard quit exits the process immediately using its exit code, discarding any following instruction flags.
- `NPa Q` breaks *a* levels of nested macro execution. `1Q` ends the macro it's in (mostly useless), `2Q` breaks the macro that called it, and so on.

There are some commands that interact with the OS, which may be disabled with "restricted mode" (**CLI:** `-r` flag).
- gleeb
- zorp
- etc
- TODO


# Errors

Runtime errors fall into 2 categories, distinguished by `!` and `?`. These are printed to the "error" stream unless Quiet mode is enabled.
- Syntax error: `! Invalid command: 💀 (U+1F480)`
- Value error: `? /: Can't divide by zero`

Error display modes/levels can be switched dynamically:
- `_quiet`: Errors are not displayed at all, error stream is effectively disabled.
- `_error`: Normal error display.
- `_debug`: Displays one-line reports of every executed command.
- These options persist for one interpreter instance. **CLI:** Set to `_error` by default unless a `-d` or `-q` flag is set.

If an error occurs, no action is performed. For value errors, this means that the faulty values are returned to the stack.

To enable dynamic error handling, there is an "error pointer" mechanism:
- When an error occurs, the offending command is saved and a counter is set to 1. Every command parsed after that will increment the counter, until the error is cleared or overwritten by a new one. This functionality does not depend on the error display mode.
- `E -> (NPx SCy Sz)` reads and clears the error. *x* indicates how many commands ago the (most recent) error happened, *y* is the offending command, and *z* is the error message.
- This also persists for one interpreter instance.