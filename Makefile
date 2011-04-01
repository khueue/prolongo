PROLOG    = swipl -O
PROLOG_LD = swipl-ld
CC        = gcc
CFLAGS    = -cc $(CC) -Wall -Wextra -ansi -pedantic -O4

.PHONY: all
all: trim compile test

# Remove trailing whitespace and such. Not important.
.PHONY: trim
trim:
	clear
	@- trim *.md *.pl src/*.pl src/bson/*.pl src/mongo/*.pl ext/*.c

.PHONY: test
test: compile
	@ echo "--- Running tests and exiting ..."
	$(PROLOG) -g "[load], call_cleanup(test, halt)"

.PHONY: stay
stay: compile
	@ echo "--- Running tests ..."
	$(PROLOG) -g "[load], test"

.PHONY: compile
compile: setup lib/bson_bits

# Generic name (not sure what file extensions different systems use).
lib/bson_bits: ext/bson_bits.c Makefile
	@ echo "--- Compiling foreign library 'bson_bits' ..."
	$(PROLOG_LD) -shared -o $@.dylib ext/bson_bits.c $(CFLAGS)
	mv $@.dylib $@

.PHONY: setup
setup: lib

lib:
	mkdir -p lib

.PHONY: clean
clean:
	rm -rf lib/*
