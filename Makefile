LIB = $(PWD)/build/lib
BIN = build/bin
SRC = src
UTILFLAGS = -I include -L $(LIB) -lbrb -O3 -Wno-nullability-completeness
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

libbrb: src/brb_*.c
	cc -c $(SRC)/brb_*.c -Wno-initializer-overrides -I include
	cc -shared -o $(LIB)/libbrb.dylib brb_*.o
	rm brb_*.o

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
