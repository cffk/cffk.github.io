# This builds a tarball
# This probably won't be of interest to most end users.

all: tarballs

.PHONY: tarballs all allclean clean

TAR=gtar
GZIP=gzip
tarballs: allclean
	cd ..; \
	$(TAR) -c -f random/random-`cat random/VERSION`.tar --exclude='*.tar*' random
	$(GZIP) -c random-`cat VERSION`.tar > random-`cat VERSION`.tar.gz

allclean: clean
	(cd doc; $(MAKE); $(MAKE) clean)
	(cd src; $(MAKE); $(MAKE) clean)
	(cd test; $(MAKE) sources; $(MAKE) clean)

clean:
	rm -f *.o *.F *.tex *.aux* *.log *.ind *.toc *.sty *.out *.mod *~
