
DESTDIR=../../www/notes

MDFILE := $(word 1, $(basename $(wildcard *.md)))
L ?= $(MDFILE)

all: $(L).html $(L)-slides.html
.PHONY: all

$(L).html: $(L).md
	@test -f $<
	pandoc --data-dir=../pandoc -s -t html -o $@ $<

$(L)-slides.html: $(L).md $(wildcard ../pandoc/slidy/*)
	@test -f $<
	pandoc --data-dir=../pandoc --offline -s -t slidy -o $@ $<

CONFIG := $(dir $(PWD))config
show: $(L)-slides.html
	XDG_CONFIG_HOME=$(CONFIG) XDG_DATA_HOME=$(CONFIG)/share \
		HOME=$(CONFIG) xdg-open $< &
.PHONY: show

install: $(L).md $(L).html $(L)-slides.html
	cp $^ $(wildcard *.svg) $(DESTDIR)/
	-git add $(patsubst %, $(DESTDIR)/%, $^ $(wildcard *.svg))
.PHONY: install

clean:
	rm -f $(L).html $(L)-slides.html *.o *.hi *~ \
		$(patsubst %.hs,%,$(wildcard *.hs))
.PHONY: clean
