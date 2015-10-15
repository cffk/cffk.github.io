# $Id: Makefile 6001 2005-06-23 15:51:30Z ckarney $
# (c) 2004, Sarnoff Corporation, Princeton, NJ, USA

TARGETS=g96topdb addhydrogens gjftogms gmstoresp chargesym gmstobcc divcon acreorder antechamberfilt babelfilt gjftogcrt gjfrenumber

all: $(TARGETS)
	install divcon $(AMBERHOME)/exe

%: %.sh
	cp $< $@
	chmod +x $@

distrib:
	tar cfz b2dscripts-r1.tar.gz Makefile README.txt LICENSE.txt \
		index.html gromacs.patch \
		$(addsuffix .sh,$(TARGETS))
