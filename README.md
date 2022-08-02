# BRidge

BRidge is a procedural and object-oriented statically typed language with the ability to run both natively and in its own runtime environment.

## Getting started

To build all of the BRidge toolchain, simply run the following command:
```console
$ make
```

## Tools
- [bridge](src/bridge.c) - The main tool for everything about the language; It's a compiler, assembler and interpreter, all in one
- [brbd](src/brbd.c) - BRidge Bytecode Debugger, a tool for debugging BRB modules

## Documentation

- [BRidge Assembly Documentation](docs/brb.md).

## Examples

### VBRB example
'Hello World' program in BRidge assembly is [there](examples/hello_world.vbrb) and can be compiled and executed with these commands:
To execute bytecode, run:
```console
$ brs examples/hello_world.vbrb
$ brbx examples/hello_world.brb
```
To execute native executable, run:
```console
$ brs examples/hello_world.vbrb
$ brbc examples/hello_world.brb
$ examples/hello_world
```

### BRidge examples
'Hello World' program in BRidge is [there](examples/hello_world.br) and can be compiled and executed using the following commands:
To execute in the BRidge runtime, run:
```
$ brc -r examples/hello_world.br
```
P.S.: the `-r` flag makes the compiler execute the compiled program with `brbx` immediately after it's compiled

To compile the program to a native executable, run:
```
$ brc examples/hello_world.br
$ brbc examples/hello_world.brb
$ examples/hello_world
```
