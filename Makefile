PROLOG    = swipl -O
PROLOG_LD = swipl-ld
CC        = clang
CFLAGS    = -cc $(CC) -Wall -Wextra -ansi -pedantic -O4

.PHONY: lib

all: trim lib halt

clean:
	rm -rf lib/*

trim:
	@# Remove trailing whitespace and such. Not vital.
	@- trim *.md src/*.pl src/*.c

halt:
	clear
	$(PROLOG) -g "[load], call_cleanup(run, halt)"

stay:
	clear
	$(PROLOG) -g "[load], run"

lib: lib/bson_bits

lib/bson_bits: Makefile ext/bson_bits.c
	@ mkdir -p lib
	$(PROLOG_LD) -shared -o $@.dylib ext/bson_bits.c $(CFLAGS)
	@ mv $@.dylib $@
