LIB = $(PWD)/build/lib
BIN = build/bin
SRC = src
UTILFLAGS = -I include -L $(LIB) -lbrb -O3 -Wno-nullability-completeness -ferror-limit=1
GLOBAL = /usr/local/bin

main: $(BIN) $(LIB)/libbrb.dylib $(GLOBAL)/brs $(GLOBAL)/brbx $(GLOBAL)/brbc $(GLOBAL)/brc $(GLOBAL)/brbd

$(GLOBAL)/brs: $(BIN)/brs
ifeq ("$(wildcard $(GLOBAL)/brs)", "")
	sudo ln -sF $(PWD)/$(BIN)/brs $(GLOBAL)/brs
endif

$(GLOBAL)/brbx: $(BIN)/brbx
ifeq ("$(wildcard $(GLOBAL)/brbx)", "")
	sudo ln -sF $(PWD)/$(BIN)/brbx $(GLOBAL)/brbx
endif

$(GLOBAL)/brbc: $(BIN)/brbc
ifeq ("$(wildcard $(GLOBAL)/brbc)", "")
	sudo ln -sF $(PWD)/$(BIN)/brbc $(GLOBAL)/brbc
endif

$(GLOBAL)/brc: $(BIN)/brc
ifeq ("$(wildcard $(GLOBAL)/brc)", "")
	sudo ln -sF $(PWD)/$(BIN)/brc $(GLOBAL)/brc
endif

$(GLOBAL)/brbd: $(BIN)/brbd
ifeq ("$(wildcard $(GLOBAL)/brbd)", "")
	sudo ln -sF $(PWD)/$(BIN)/brbd $(GLOBAL)/brbd
endif

$(LIB)/libbrb.dylib: src/brb_*.c
	cc -c $(SRC)/brb_*.c -Wno-initializer-overrides -I include -ferror-limit=1
	cc -shared -o $(LIB)/libbrb.dylib brb_*.o
	rm brb_*.o

$(BIN)/brs: src/brs.c
	cc $(UTILFLAGS) -o $(BIN)/brs $(SRC)/brs.c

$(BIN)brbx: src/brbx.c
	cc $(UTILFLAGS) -o $(BIN)/brbx $(SRC)/brbx.c

$(BIN)/brbc: src/brbc.c
	cc $(UTILFLAGS) -o $(BIN)/brbc $(SRC)/brbc.c

$(BIN)/brc: src/brc.c
	cc $(UTILFLAGS) -Wno-initializer-overrides -o $(BIN)/brc $(SRC)/brc.c

$(BIN)/brbd: src/brbd.c
	cc $(UTILFLAGS) -o $(BIN)/brbd $(SRC)/brbd.c
