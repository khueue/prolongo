PROLOG    = swipl -O
PROLOG_LD = swipl-ld
CFLAGS    = -Wall -Wextra -ansi -pedantic

all: trim libs run

trim:
	@# Remove trailing whitespace and such. Not vital.
	@- trim *.md src/*.pl src/*.c

run:
	clear
	@ $(PROLOG) -g "['src/load.pl'], call_cleanup(run, halt)"

stay:
	clear
	@ $(PROLOG) -g "['src/load.pl'], run"

libs:
	mkdir -p lib
	$(PROLOG_LD) -shared -o lib/bson_bits.dylib src/bson_bits.c $(CFLAGS)
	mv lib/bson_bits.dylib lib/bson_bits
