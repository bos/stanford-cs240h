
MDFILES := $(wildcard *.md)
TARGETS := $(patsubst %.md,%.html,$(MDFILES))

all: $(TARGETS)
.PHONY: all

%.html: %.md
	pandoc --data-dir=../../notes/pandoc --css=../style.css \
		-s -t html -o $@ $<

clean:
	rm -f *~
.PHONY: clean
