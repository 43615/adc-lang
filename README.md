# ADC: Array-oriented reimagining of dc, a terse stack-based esolang

The goal of this project is to introduce polymorphic array support to an ancient scriptable RPN calculator program called `dc` (desk calculator), while preserving its elegant core tenets.

It takes the form of a highly performant interpreter that can be used interactively and/or with scripts. Potential applications include precise and complex mathematical calculations, string-based data processing, and lightweight interactive scripts combining both.

The nature of the language will seem unusual to those familiar with most "normal" programming languages due to the following aspects:
- Commands (instructions) are generally *single characters* and perform quite basic operations. This makes scripts very compact, but also hard to read for the uninitiated.
- Instead of named variables, data is stored on various *stacks*. This naturally leads to an RPN-like syntax for every operation, which has the benefit of completely linear execution (no backtracking or looking ahead). In effect, this is quite similar to machine code or assembly languages.