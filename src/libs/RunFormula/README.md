# RunFormula
RunFormula is intended to evaluate mathematical expressions and physics formulas provided as text with scripting language support and physical dimension handling.

Supported data types:
* integers in decimal, hexadecimal and binary formats;
* floating-point numbers in decimal and scientific notation;
* complex numbers and operations on them;
* intervals and operations on them;
* strings and ASCII characters;

Arithmetic and logical operations: + - * / or and xor not shl (<<) shr (>>) mod (%) div == <> < <= >= > & (string concatenation) ** (integer exponentiation).\
Variables and runtime initialization of variables via an external callback function.\
A base set of built-in functions and the ability to register and use additional user-defined functions.\
Control flow functions: if() repeat() exit() result() continue() break()\
Ability to define and subsequently use functions directly within the formula text (subroutines).\
Support for physical dimensions, their runtime checking, automatic calculation of the resulting dimension, and automatic conversion of values between different unit systems.\
Support for the define directive.\
Compiling the source formula into bytecode for multiple execution.\
Minimal dependencies (only SysUtils in the base configuration).

#### Integrating RunFormula into your project
- Copy all files from the `RunFormula` directory (`runformula.pas` and all `.inc` files) to your project or a separate directory.
- Add `RunFormula` to the `uses` clause of either the `interface` or `implementation` section.

For example, like this:
```
implementation
uses RunFormula;
```

or, if using a separate directory:
```
implementation
uses RunFormula in 'RunFormula/runformula.pas';
```

#### How to use RunFormula
The simplest way is to call `RunFlaParse` function and then pass its output to the `RunFlaExecStr` or `RunFlaExecVrt` functions.\
For example:
```
ShowMessage( RunFlaExecStr( RunFlaParse('9 * 3') ) );  // displays 27
```
or, with physical units:
```
ShowMessage( RunFlaExecStr( RunFlaParse('g=9.8`N/kg`, t=5, g*t`s`**2/2') ) );  // displays 122.5`m`
```
`RunFlaParse` compiles the source formula into bytecode for execution by `RunFlaExecStr` or `RunFlaExecVrt`. The result of `RunFlaExecStr` is a string,
while `RunFlaExecVrt` returns a Variant.\
See the corresponding help topics for advanced usage at [Help/Eng/runflahelp-eng.txt](https://github.com/runnfla/RunFormula/blob/main/Help/Eng/runflahelp-eng.txt) ([Help/Rus/runflahelp.txt](https://github.com/runnfla/RunFormula/blob/main/Help/Rus/runflahelp.txt)  in Russian).

--\
**Author:** Alexander Torubarov\
**Contact:** runfla@yandex.com

Copyright (C) 2026 Alexander Torubarov\
Licensed under the MIT License.\
See the `LICENSE` file in the project root or a copy available at [opensource.org](https://opensource.org) for full license information.
