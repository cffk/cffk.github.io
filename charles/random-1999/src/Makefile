# This builds the random.f and the documentation from the web files.
# This probably won't be of interest to most end users.

all: sources

.PHONY: sources clean all reallyclean

clean:
	rm -f *.o *.x *.y *.tex *.aux* *.log *.ind *.toc *.sty *.out *.mod *~

SOURCEDIR=/u/karney/degas2/src
TANGLE=ftangle
TANGLEFLAGS=-W[ -n/ -n')' -Tv -j -yn7000 -yb100000

define CLEANUP
    ( echo "! Version `cat ../VERSION` of random number routines"; \
    echo "! Author: Charles Karney <karney@princeton.edu>"; \
    echo "! Date: `date '+%Y-%m-%d %H:%M %z'`;!"; \
    sed -e 's/^\*.*//' -e 's/^[cC].*//' $< ) | \
    sed -e 's/ *$$//' | sed -e 's/^ *if.*continue//' | grep -v '^ *$$' | \
    sed -e 's/ran_array_x/ra/g'  -e 's/ran_index_x/ri/g' | \
    sed -e 's/ran_//g' | tr '\n' ';' | sed -e 's/;     &//g' | \
    sed -e 's/end;/end;!;/g' | tr ';' '\n' > $@
endef

SOURCES=random.f random-hiprec.f random.F randother.f randother-hiprec.f randother.F
HEADERS=random.hweb macros.hweb sysdep.hweb vector.hweb constants.hweb array.hweb

sources: $(SOURCES)

vpath %.web $(SOURCEDIR)
vpath %.hweb $(SOURCEDIR)

%.x: %.web $(HEADERS)
	$(TANGLE) $(TANGLEFLAGS) -mSUN -=n=#.x $<

%-hiprec.x: %.web $(HEADERS)
	$(TANGLE) $(TANGLEFLAGS) -mCRAY -mUSEINT -=n=#-hiprec.x  $<

%.f: %.F

%.f: %.x
	$(CLEANUP)

%.y: %.f
	sed -e 's/REAL/REAL8/g' -e 's/DOUBLE PRECISION/REAL8/g' \
	-e 's/1.0[de]0/ONE/g' -e 's/2.0[de]0/TWO/g' \
	-e 's/3.14159265358979323846264338328[de]0/PI/g'  $< > $@

random.F: random.y random-hiprec.y
	-(echo "#include <config.h>"; diff -DHIPREC random.y random-hiprec.y \
	|  sed -e 's/^      \(.*\)=tmp-int(tmp)$$/#if USEINT;      \1=tmp-int(tmp);#else;      if(tmp.GE.ONE)then;      \1=tmp-ONE;      else;      \1=tmp;      end if;#endif/' \
	| tr ';' '\n' ) > $@

randother.F: randother.y randother-hiprec.y
	-(echo "#include <config.h>"; diff -DHIPREC randother.y randother-hiprec.y) > $@

reallyclean: clean
	rm -f $(SOURCES)
