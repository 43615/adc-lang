# ADC: Array-oriented reimagining of dc, a terse stack-based esolang

This is a project to introduce array programming paradigms and other improvements to an ancient calculator language called [dc](https://en.wikipedia.org/wiki/dc_(computer_program)), while preserving its core tenets:
- Very compact syntax, usually single-character commands.
- Operations implicitly occur on a stack, resulting in an RPN syntax.
- Straightforward, purely procedural execution flow using strings as code.
- Interpreter performance comparable to a compiled language.

Improvements over dc:
- Array support with arbitrary nesting (dimensions). In general, the primitive functions are implicitly applied to entire arrays at once.
- Boolean vectors as a new type, enabling more comfortable conditionals.
- Numbers are rationals of arbitrary size, provided by [Malachite](https://www.malachite.rs/).
- Strings have actual manipulation commands and full UTF-8 support. Includes regex find/replace operations.
- Additional arithmetic functions, including real-valued ones (performed using floats).
- Arbitrary input/output bases for numbers, with a special format above base 36. Stack for parameter contexts.
- Registers with arbitrary indices, directly selectable with a number.
- Macro execution in separate threads using registers as handles.
- Expanded command line argument syntax, including saving/loading the whole interpreter state.
- OS-interfacing features, such as running commands and file I/O. May be restricted with a flag.

General principles of this implementation:
- Adherence to the [ZOI rule](https://en.wikipedia.org/wiki/Zero_one_infinity_rule), though obviously with an exception for pointers where required.
- Algorithms with optimal performance for large inputs, even at the cost of some flat inefficiency for small inputs. This is also the case for Malachite's arithmetic functions.
- Array-related functions support arbitrary nesting by using iterative algorithms instead of recursive calls.
- No use of generative AI of any kind, all of this is made by humans.