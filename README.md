kodellama2
==========

Interpreter for the Kodellama language that we have created. Project for CS4240 Fall 2014 at UVA.
This interpreter covers a subset of the Kodellama language, which we designed to be accessible to those with little programming experience. The language is meant to be used in conjunction with a development environment and runtime that allow the user to interact with their program to see what's happening, facilitating teaching; these pieces will be added at a later time. This repository contains the language core.

This readme first describes the dependencies of Kodellama and how to build and run it (Section I), and then provides a short overview of the current state of the language (Section II).

- Scelerus and MattIrv

==========
SECTION I
==========

Building & Running

Kodellama requires Coq 8.4 or higher and Ocaml 3.12 or higher (although it may work on older versions, it has not been tested). It has only be tested on linux and mac osx, but may also work on windows.

To build Kodellama, run "make" in the top level directory of the repository. You can then run "make test" to have Kodellama run the set of sample programs contained in the "sampleprograms" directory and compare them to the "golden" output versions contained there. Kodellama should pass all of these tests.

To use the interpreter from the command line, use the Kodellama executable created by running "make". For example, type "./Kodellama" from your terminal after navigating to the toplevel directory of the Kodellama repository. This will present you with an interpreter; type your program and then Control-D to terminate input.

You may also write a Kodellama program in your text editor of choice and then pipe it to the interpreter by typing "./Kodellama < myprogram.kdl" where "myprogram.kdl" is the name of your program (.kdl is the Kodellama file extension used by older versions of the language). 

==========
SECTION II
==========

Language overview

In what follows, (words in parentheses) indicate that anything of that type is acceptable, and [words in brackets] denote optional constructions.

Kodellama programs are made up of a series of commands, of which the following are available:
  - skip : the simplest command, skip makes the program do nothing
  - if (boolean expression) then (command) [else (command)] : if the (boolean expression) evaluates to true, then the program runs the command (or commands - they need to be separated by either newlines or semicolons) in the "then" section. If not, it will run the command in the "else" section if it exists.
  - while (boolean expression) do (command) end : while (boolean expression) remains true, the (command) in the loop body will continue to be evaluated.
  - repeat (arithmetic expression) times: (command) end : runs the (command) however many times (arithmetic expression) evaluates to when the loop is first run
  - set (variable) to (value) : sets (variable) so that whenever it is referenced in following commands it has (value). Can be overwritten by a following set or let.
  - let (variable) be (value) : sets (variable) so that it always refers to value. It becomes an error to try to set or let this (variable).
  - match (expression) with (value1): (command) with (value2): ... end : evaluates (expression) and tries to find the pattern among the (value) expressions which it matches. Once it has found a matching value, it executes that command.
  - print (expression) : prints whatever value (expression) currently has

Expressions are either boolean, string, or arithmetic expressions.

A boolean expression can include:
  - the constants "true" and "false"
  - a variable with a boolean value
  - boolean "and" and "or"
  - parentheses
  - boolean "not"

A string expression can include:
  - string constants, which are surrounded by quotes, like this: "Hello, world"
  - string valued variables
  - the string concatenation operator "+", which takes the strings on either side of it and makes one bigger string

An arithmetic expression can include:
  - numeric literals, which may be either integers or floating point numbers (i.e. numbers with decimal points, like 5.3)
  - the basic mathematical operators: "+" for plus, "-" for minus, "*" for multiplication, and "/" for division
  - numeric valued variables
  - parentheses
  - the advanced mathematical operators: "^" for exponentiation and "%" for modular division (i.e. remainder). Note that these are not currently implemented and will both always return either zero or an error.
