LIB = $(PWD)/build/lib
BIN = build/bin
SRC = src
UTILFLAGS = -I include -L $(LIB) -lbrb -O3
GLOBAL = /usr/local/bin

main: libbrb brs brbx brbc brc brbd
ifeq ("$(wildcard $(GLOBAL)/brs)", "")
	sudo ln -sF $(PWD)/$(BIN)/brs $(GLOBAL)/brs
endif
ifeq ("$(wildcard $(GLOBAL)/brbx)", "")
	sudo ln -sF $(PWD)/$(BIN)/brbx $(GLOBAL)/brbx
endif
ifeq ("$(wildcard $(GLOBAL)/brbc)", "")
	sudo ln -sF $(PWD)/$(BIN)/brbc $(GLOBAL)/brbc
endif
ifeq ("$(wildcard $(GLOBAL)/brc)", "")
	sudo ln -sF $(PWD)/$(BIN)/brc $(GLOBAL)/brc
endif
ifeq ("$(wildcard $(GLOBAL)/brbd)", "")
	sudo ln -sF $(PWD)/$(BIN)/brbd $(GLOBAL)/brbd
endif

libbrb: src/brb.c src/vbrb.c src/core.c
	cc -Wno-tautological-constant-out-of-range-compare -O3 -c -o $(LIB)/core.o -I include $(SRC)/core.c
	cc -c -o $(LIB)/brb.o -I include -O3 $(SRC)/brb.c
	cc -c -o $(LIB)/vbrb.o -I include -O3 $(SRC)/vbrb.c
	cc -shared -o $(LIB)/libbrb.dylib $(LIB)/core.o $(LIB)/vbrb.o $(LIB)/brb.o

brs: src/brs.c
	cc $(UTILFLAGS) -o $(BIN)/brs $(SRC)/brs.c

brbx: src/brbx.c
	cc $(UTILFLAGS) -o $(BIN)/brbx $(SRC)/brbx.c

brbc: src/brbc.c
	cc $(UTILFLAGS) -o $(BIN)/brbc $(SRC)/brbc.c

brc: src/brc.c
	cc $(UTILFLAGS) -Wno-initializer-overrides -o $(BIN)/brc $(SRC)/brc.c

brbd: src/brbd.c
	cc $(UTILFLAGS) -o $(BIN)/brbd $(SRC)/brbd.c
