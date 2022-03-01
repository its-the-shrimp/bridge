# BRidge

BRidge is a compiler ecosystem as well as an execution environment.
Yet to be cross-platform tho, it's still very early in development.

## Getting started

To build all of the BRidge toolchain, run the following command
```console
$ cc -o nobuild nobuild.c
$ ./nobuild
```

## Tools
- [brex](src/brex.c) - Execution environment for BRidge bytecode.
- [brf](src/brf.c) - BRidge assembler, translates BRidge assembly `.vbrf` files to executable bytecode `.brf` files.

## Example
'Hello World' in BRidge assembly is [there](./hello_world.vbrf) and can be compiled and executed with these commands:
```console
$ brf hello_world.vbrf
$ brex hello_world.brf
```