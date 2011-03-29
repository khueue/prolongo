PROLOG = swipl -O

all: trim run

trim:
	@# Remove trailing whitespace and such. Not vital.
	@- trim *.md src/*.pl

run:
	clear
	@ $(PROLOG) -g "['src/load.pl'], run"

halt:
	clear
	@ $(PROLOG) -g "['src/load.pl'], call_cleanup(run, halt)"
