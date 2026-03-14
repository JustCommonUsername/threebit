# Three Bit Language Interpreter
This is a simple Scala project with a three bit language for the JetBrains internship.

## Structure

The main file is `ThreeBit.scala`. The `Parser` object types the program, and `Interpreter` runs
the program using the typed instructions.

## Usage

First line is the value of register `X`, second of `Y`, third of `Z`, and fourth is the program.
Example:

```bash
$ sbt run
3729
0
0
0,1,5,4,3,0
```

Output for that example: `0,4,2,1,4,2,5,6,7,3,1,0`

Simple unit tests are in `ThreeBitUnitTest.scala`.

## Operational Semantics

Here I write operational semantics of this language, which I wrote from the task description and
used to implement interpreter.

[](JetBrains%20Internship%20Application,%20ThreeBit%20Operational%20Semantics.jpg)

Sorry for handwritten notes, I'll tex them later.

