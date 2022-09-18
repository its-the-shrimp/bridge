# BRidge

BRidge is a procedural and object-oriented statically typed language with the ability to run both natively and in its own runtime environment.

## Getting started

To build all of the BRidge toolchain, simply run the following command:
```console
$ python3 build.py --release
```

## Tools
- [bridge](src/brc.c) - The main tool for everything about the language; It's a compiler, assembler, disassembler and interpreter, all in one

## Documentation
Not yet :(

## Examples

### VBRB example
'Hello World' program in BRidge assembly is [there](examples/hello_world.vbrb) and can be compiled and executed with these commands:
To execute bytecode, run:
```console
$ bridge -Ai examples/hello_world.vbrb
```
To execute native executable, run:
```console
$ bridge -Ax examples/hello_world.vbrb
$ examples/hello_world
```

### BRidge example
'Hello World' program in BRidge is [there](examples/hello_world.br) and can be compiled and executed using the following commands:
To execute in the BRidge runtime, run:
```console
$ bridge -Ai examples/hello_world.br
```
P.S.: the `-Ai` flag makes the compiler behave like an interpreter

To compile the program to a native executable, run:
```console
$ bridge -Ax examples/hello_world.br
$ examples/hello_world
```
