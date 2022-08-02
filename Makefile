LIB = $(PWD)/build/lib
BIN = build/bin
SRC = src
UTILFLAGS = -I include -L $(LIB) -lbrb -O3 -Wno-nullability-completeness -ferror-limit=1
GLOBAL = /usr/local/bin

main: $(BIN)/* $(LIB)/libbrb.dylib $(GLOBAL)/bridge $(GLOBAL)/brbd

$(GLOBAL)/bridge: $(BIN)/bridge
ifeq ("$(wildcard $(GLOBAL)/bridge)", "")
	sudo ln -sF $(PWD)/$(BIN)/bridge $(GLOBAL)/bridge
endif

$(GLOBAL)/brbd: $(BIN)/brbd
ifeq ("$(wildcard $(GLOBAL)/brbd)", "")
	sudo ln -sF $(PWD)/$(BIN)/brbd $(GLOBAL)/brbd
endif

$(LIB)/libbrb.dylib: src/brb_*.c include/*
	cc -c $(SRC)/brb_*.c -Wno-initializer-overrides -I include -ferror-limit=1
	cc -shared -o $(LIB)/libbrb.dylib brb_*.o
	rm brb_*.o

$(BIN)/bridge: src/brc.c
	cc $(UTILFLAGS) -Wno-initializer-overrides -o $(BIN)/bridge $(SRC)/brc.c

$(BIN)/brbd: src/brbd.c
	cc $(UTILFLAGS) -o $(BIN)/brbd $(SRC)/brbd.c
