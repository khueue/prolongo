# Flags: skip welcome, optimize, run goal (expects argument).
PROLOG = swipl --quiet -O -g

all: trim

trim:
	@# Remove trailing whitespace and such. Not vital.
	- trim *.md **/*.pl

run:
	$(PROLOG) "['src/*.pl'], halt"
