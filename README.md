# BRidge

BRidge is a compiler ecosystem as well as an execution environment, providing exhaustive and verbose debugging and error checking.
Yet to be cross-platform tho, it's still very early in development.

## Getting started

To build all of the BRidge toolchain, run the following command
```console
$ cc -o nobuild nobuild.c
$ ./nobuild
```

## Tools
- [brbx](src/brbx.c) - BRidge Bytecode eXecution environment. Interprets operations from a `.brb` file, can also provide exhaustive debugging abilities.
- [brbc](src/brbc.c) - BRidge Bytecode Compiler, compiles `.brb` files to native assembly, object or executable file.
- [brs](src/brs.c) - BRidge aSsembler, translates BRidge assembly `.vbrb` files to executable bytecode `.brb` files.

## Documentation

- [BRidge Assembly Documentation](docs/brb.md).

## Example
'Hello World' in BRidge assembly is [there](examples/hello_world.vbrb) and can be compiled and executed with these commands:
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
