# This builds the documentation from the web files.
# This probably won't be of interest to most end users.

all: docs

.PHONY: all docs clean reallyclean

SOURCEDIR=/u/karney/degas2/src
TANGLE=ftangle
TANGLEFLAGS=-W[ -n/ -n')' -Tv -j -yn7000 -yb100000
LATEX=latex
PDFLATEX=pdflatex
WEAVE=fweave
WEAVEFLAGS=-f -i! -W[ -ykw600 -ytw40000 -j -n/

define RUN-LATEX
  if [ -r $*.aux ]; then cp $*.aux $*.aux0; fi
  $(LATEX) $*.tex
  if [ -r $*.aux ]; then cmp -s $*.aux $*.aux0 || $(LATEX) $*.tex; fi
  rm -f $*.aux0
endef

define RUN-PDFLATEX
  if [ -r $*.aux ]; then cp $*.aux $*.aux0; fi
  $(PDFLATEX) $*.tex
  if [ -r $*.aux ]; then cmp -s $*.aux $*.aux0 || $(PDFLATEX) $*.tex; fi
  rm -f $*.aux0
endef

DOCS=random.dvi random.ps random.pdf randother.dvi randother.ps randother.pdf
HEADERS=random.hweb

docs: $(DOCS)

vpath %.web $(SOURCEDIR)
vpath %.hweb $(SOURCEDIR)

%.sty: $(SOURCEDIR)/%.sty
	cp $< $@

%.dvi: %.tex
%.dvi: %.tex fweboptions.sty fweb.sty
	$(RUN-LATEX)

%.tex: %.web
%.tex: %.web fweb.sty $(HEADERS)
	$(WEAVE) $(WEAVEFLAGS) $<

%.ps: %.dvi
	dvips -Pwww -f $^ > $@

%.pdf: %.tex fweboptions.sty fweb.sty
	$(RUN-PDFLATEX)

clean:
	rm -f *.o *.F *.tex *.aux* *.log *.ind *.toc *.sty *.out *.mod *~

reallyclean: clean
	rm -f $(DOCS)
