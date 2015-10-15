#! /bin/sh

# $Id: divcon.sh 6001 2005-06-23 15:51:30Z ckarney $
# (c) 2004, Sarnoff Corporation, Princeton, NJ, USA

# Dummy divcon executable to allow antechamber to use charges from
# Gamess.  It ignores the input file divcon.in.  If environment variable
# DIVCONOUT is defined it copies this to divcon.out.  Otherwise it does
# nothing (presumably divcon.out already exists).

[ "$DIVCONOUT" ] && cp "$DIVCONOUT" divcon.out

exit
