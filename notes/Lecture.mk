
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

show: $(L)-slides.html
	xdg-open $<
.PHONY: show

clean:
	rm -f $(L).html $(L)-slides.html *.o *.hi *~ \
		$(patsubst %.hs,%,$(wildcard *.hs))
.PHONY: clean
