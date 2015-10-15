TARGETS=testrandom pirandom
HIPRECTARGETS=testrandom-hiprec pirandom-hiprec
.PHONY: sources clean all reallyclean

all: $(TARGETS)
hiprec: $(HIPRECTARGETS)
sources: testrandom-hiprec.f pirandom-hiprec.f

MPIFC=mpif77
LIBDIR=../src
COPTS=-c -o

vpath %.f $(LIBDIR)
vpath %.h $(LIBDIR)

%.o: %.f
	$(FC) $(FFLAGS) $(COPTS) $@ $<

testrandom.o: testrandom.f random.h
	$(FC) $(FFLAGS) -I$(LIBDIR) $(COPTS) $@ $<

testrandom: testrandom.o random.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^

pirandom.o: pirandom.f random.h
	$(MPIFC) $(FFLAGS) -I$(LIBDIR) $(COPTS) $@ $<

pirandom: pirandom.o random.o
	$(MPIFC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(MPILIBS)

%-hiprec.f: %.f
	sed -e 's/double precision/real/g' -e 's/d0/e0/g' $< > $@

testrandom-hiprec.o: testrandom-hiprec.f random.h
	$(FC) $(FFLAGS) -I$(LIBDIR) $(COPTS) $@ $<

testrandom-hiprec: testrandom-hiprec.o random-hiprec.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^

pirandom-hiprec.o: pirandom-hiprec.f random.h
	$(MPIFC) $(FFLAGS) -I$(LIBDIR) $(COPTS) $@ $<

pirandom-hiprec: pirandom-hiprec.o random-hiprec.o
	$(MPIFC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(MPILIBS)

clean:
	rm -f *.o *.F *.tex *.aux* *.log *.ind *.toc *.sty *.out *.mod *~
	rm -f mpif.h $(TARGETS) $(HIPRECTARGETS)
