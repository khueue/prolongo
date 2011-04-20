PROLOG    = swipl -O
PROLOG_LD = swipl-ld
CC        = gcc
CFLAGS    = -cc $(CC) -Wall -Wextra -ansi -pedantic -O4 -fPIC

.PHONY: all
all: trim compile test

# Remove trailing whitespace and such. Not important.
.PHONY: trim
trim:
	clear
	@- trim *.md *.pl* src/*.pl* ext/*.c src/bson/*.pl* src/mongo/*.pl*

.PHONY: test
test: compile
	@ echo "--- Run tests and exit ..."
	$(PROLOG) -s load -g test -t halt

.PHONY: test
bench: compile
	@ echo "--- Run benchmark ..."
	$(PROLOG) -s load -g bench -t halt

.PHONY: cov
cov: compile
	@ echo "--- Run tests, print test coverage and exit ..."
	$(PROLOG) -s load -g cov -t halt

.PHONY: repl
repl: compile
	@ echo "--- Load and enter REPL ..."
	$(PROLOG) -s load -g repl

.PHONY: doc
doc: compile
	@ echo "--- Generate docs ..."
	$(PROLOG) -s load -g doc -t halt

#latex -output-directory=doc -output-format=pdf doc/*.tex

.PHONY: compile
compile: setup lib/bson_bits

# Generic name (not sure what file extensions different systems use).
lib/bson_bits: ext/bson_bits.c Makefile
	@ echo "--- Compile foreign library 'bson_bits' ..."
	rm -f $@
	$(PROLOG_LD) -shared -o $@.dylib ext/bson_bits.c $(CFLAGS)
	mv $@.dylib $@

.PHONY: setup
setup: lib

lib:
	mkdir -p lib

.PHONY: clean
clean:
	rm -rf lib/*
	rm -rf doc/src
