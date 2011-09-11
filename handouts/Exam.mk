#
# Must define MAIN in file that includes this (e.g., MAIN = q1.ltx)
# Can define GENFIGS to any extra figures that need to be generated
# and GENFIGSRC to any source
# Set PDF=pdf to use pdflatex

ifeq ($(PDF),pdf)
FIGEXT    = pdf
OUTEXT    = pdf
else
FIGEXT    = eps
OUTEXT    = dvi
endif

BASE      := $(basename $(MAIN))
SRCEXT    := $(patsubst $(BASE).%,%,$(MAIN))
XFIGS     := $(wildcard *.fig)
DIAFIGS   := $(wildcard *.dia)
TEXFIGS   := $(XFIGS:.fig=.tex) # $(DIAFIGS:.dia=.tex)
BINFIGS   := $(XFIGS:.fig=.$(FIGEXT)) $(DIAFIGS:.dia=.$(FIGEXT)) $(GENFIGS)
TEXFILES  := $(sort $(MAIN) $(wildcard *.tex) $(TEXFIGS))
STYFILES  := quiz.cls

JGRAPHS   := $(wildcard *.j)
GGRAPHS   := $(wildcard *.gnuplot)
GRAPHS    := $(JGRAPHS.j=.$(FIGEXT)) $(GGRAPHS:.gnuplot=.$(FIGEXT))

ifeq ($(PDF),pdf)
IMAGESRC  := $(wildcard figs/*.gif) $(wildcard figs/*.eps) 
else
IMAGESRC  := $(wildcard figs/*.gif) 
endif
IMAGEGEN  := $(addsuffix .$(FIGEXT), $(basename $(IMAGESRC)))
IMAGES    := $(sort $(IMAGEGEN) $(wildcard figs/*.$(FIGEXT)))
IMAGEDIST := $(filter-out $(IMAGEGEN), $(IMAGES)) \
		$(wildcard figs/*.odp) $(wildcard figs/*.ppt)

ifeq ($(PDF),pdf)
LATEX     = pdflatex \\nonstopmode\\input
else
LATEX     = latex \\nonstopmode\\input
DVIPS     = dvips -j0 -G0 -t letter
#PS2PDF    = GS_OPTIONS=-sPAPERSIZE=letter ps2pdf -sPAPERSIZE=letter
PS2PDF    = ps2pdf
endif

all: $(BASE).pdf $(BASE)-ans.pdf

ifeq ($(PDF),pdf)
%.pdf %.tex: %.fig
	fig2dev -L pdftex -p1 $< > $*.pdf
	fig2dev -L pdftex_t -p $*.pdf $< > $*.tex

%.pdf: %.eps
	perl -ne 'exit 1 unless /\n/' $< \
		|| perl -p -i -e 's/\r/\n/g' $<
	epstopdf --outfile=$@ $<

figs/%.pdf: figs/%.gif
	convert $< $@

%.ps: %.pdf
	pdftops $< $@

else
%.ps: %.dvi
	$(DVIPS) $< -o $@

%.eps %.tex: %.fig
	fig2dev -L pstex -p1 $< > $*.eps
	fig2dev -L pstex_t -p $*.eps $< > $*.tex
endif

%.eps: %.dia
	unset DISPLAY; dia -n -e $@ $<
%.tex: %.dia
	unset DISPLAY; dia -n -e $@ $<
%.eps: %.j
	@rm -f $@~
	jgraph $< > $@~
	mv -f $@~ $@
%.eps: %.gnuplot %.data
	gnuplot $*.gnuplot

$(STYFILES):
	rm -f $@
	ln -s ../$@ .

RERUN = egrep -q '(^LaTeX Warning:|\(natbib\)).* Rerun' $(basename $@).log
UNDEFINED = egrep -q '^(LaTeX|Package natbib) Warning:.* undefined' \
	$(basename $@).log

$(BASE).$(OUTEXT) $(BASE)-ans.$(OUTEXT): \
%.$(OUTEXT): %.$(SRCEXT) $(TEXFILES) $(STYFILES) $(BINFIGS) $(GRAPHS) $(IMAGES)
	$(LATEX) $< || ! rm -f $@
	! $(UNDEFINED) || $(LATEX) $< || ! rm -f $@
	! $(RERUN) || $(LATEX) $< || ! rm -f $@
	! $(RERUN) || $(LATEX) $< || ! rm -f $@
ifeq ($(PDF),pdf)
	touch $*.dvi
	test ! -f .xpdf-running || xpdf -remote $(BASE)-server -reload
else

$(BASE).pdf $(BASE)-ans.pdf: %.pdf: %.ps
	$(PS2PDF) $< $@
	test ! -f .xpdf-running || xpdf -remote $(BASE)-server -reload
endif

$(BASE)-ans.$(SRCEXT): ../Exam.mk
	@rm -f $@~
	echo '\def\doanswers{root}' > $@~
	echo '\input{$(MAIN)}' >> $@~
	mv -f $@~ $@

DIST = GNUmakefile $(MAIN) $(XFIGS) $(DIAFIGS) $(GENFIGSRC) \
	$(JGRAPHS) $(GGRAPHS) $(GGRAPHS:.gnuplot=:.data) \
	$(IMAGESRC) $(IMAGEDIST) $(STYFILES)
dist: $(BASE).tar.gz
$(BASE).tar.gz: $(DIST) ../Exam.mk
	tar -chzf $@ -C .. Exam.mk \
		$(patsubst %, $(notdir $(PWD))/%, $(DIST))

# Create a file called .noans for make preview not to show answers
PREVIEW := $(BASE)$(shell test -f .noans || echo -ans)
preview: $(PREVIEW).pdf
	if test -f .xpdf-running; then			\
		xpdf -remote $(BASE)-server -quit;	\
		sleep 1;				\
	fi
	touch .xpdf-running
	(xpdf -remote $(BASE)-server $(PREVIEW).pdf; rm -f .xpdf-running) &

show: $(BASE).pdf
	xpdf -fullscreen $(BASE).pdf

osx: $(BASE).pdf
	open $(BASE).pdf

#EXTRAIGNORE = '*.aux' '*.bbl' '*.blg' '*.log' '*.dvi' '*.bak' '*.out'
ignore:
	rm -f .gitignore
	(for file in $(EXTRAIGNORE) \
			.xpdf-running \
			$(BASE).tar.gz \
			$(TEXFIGS) $(BINFIGS) $(GRAPHS) $(IMAGEGEN) \
			$(BASE).ps $(BASE).pdf '$(BASE)-ans.*' .noans; do \
		echo "/$$file"; \
	done; \
	for file in $(STYFILES); do \
		test -r $$file -a ! -h $$file || echo /$$file; \
	done) | sort -u > .gitignore
	git add .gitignore

clean:
	rm -f $(BASE).ps $(BASE).pdf $(BASE).tar.gz $(BASE)-ans.*
	rm -f $(TEXFIGS) $(BINFIGS) $(GRAPHS) $(IMAGEGEN)
	for sty in $(STYFILES); do \
		test ! -h $$sty -o ! -f ../$$sty || rm -f $$sty; \
	done
	rm -f *.dvi *.aux *.log *.bbl *.blg *.lof *.lot *.toc *.bak *.out
	rm -f *~ *.core core

always:
	@:

.PHONY: install clean all always ignore preview show osx dist
