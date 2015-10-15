#! /bin/sh
ID='$Id: acreorder.sh 6001 2005-06-23 15:51:30Z ckarney $'

usage="
$ID
(c) 2004, Sarnoff Corporation, Princeton, NJ, USA

This shell script re-orders the atoms in an Antechamber file so that
atoms are listed in bond order (using a depth-first search).  This
enables Antechamber to create a Z-matrix representation (-fo gzmat) of
the molecule.  The starting atom can be selected and this enables
molecules to be glued together using the Z-matrix representation.

Run as a filter

    $0 [-s n] [-H] [-r] [-h] < input.ac > output.ac

Optional argument -s specifies the starting atom either as an integer
   index or as an atom label (default = 1).

Optional argument -H causes hydrogens to be treated like other atoms.
   By default the hydrogens are listed last.

Optional argument -r causes the atoms to be relabeled to reflect the new
   order.

Optional argument -h prints this message.

For more info see:
   http://amber.scripps.edu
   http://amber.scripps.edu/antechamber/antechamber.html
"

startatom=1
hlast=1
relabel=0
while getopts s:Hrh c; do
    case $c in
	s ) startatom=$OPTARG;;
	H ) hlast=0;;
	r ) relabel=1;;
        h ) echo "usage: $usage"; exit;;
        * ) echo "usage: $usage" 1>&2; exit 1;;
    esac
done
shift `expr $OPTIND - 1`

# If more than zero arguments passed
if [ $# -ne 0 ]; then
   echo "usage: $usage" 1>&2
   exit 1
fi

awk '
BEGIN { 
    natoms = 0;
    nbonds = 0;       
    startatom = 1;
    hlast = 1;			# List hydrogens last?
    relabel = 1;		# Relabel the atoms?
}
{
    if ($1 == "ATOM") {
	natoms++;
	name[natoms] = substr($0, 14, 4);
	gsub(" ", "", name[natoms]);
	if (startatom == name[natoms])
	    startatom = natoms;
	data[natoms] = substr($0, 18);
# Alphabetic portion of the name is the "Element"
	el[natoms] = substr(name[natoms], 1,
			    match(name[natoms] " ", /[^A-Za-z]/) - 1);
	nbond[natoms] = 0;	# Number of bonds to this atom
	lookup[natoms] = 0;	# The new index for this atom
	revlookup[natoms] = 0;	# Reverse lookup table
	processed[natoms] = 0;	# Have we encountered this atom in search
	count[el[natoms]] = 0;	# Initialize counts for renaming atoms
    } else if ($1 == "BOND") {	# Assume BOND lines follow all ATOM lines
	nbonds++;
	nbond[$3]++;
	nbond[$4]++;
	bonded[$3, nbond[$3]] = $4;
	bonded[$4, nbond[$4]] = $3;
	bondtype[$3, $4] = $5;
	bondtype[$4, $3] = $5;
    } else
# Print CHARGE and Formula: lines
	print $0;

}
END {
# stack for depth-first search
    stackptr = 1;
    stack[stackptr] = startatom;
    ind = 0;
    while (stackptr > 0) {
	this = stack[stackptr];
	stackptr--;
	if (processed[this] > 0)
# Already done this atom.  So skip to next atom on stack.
	    continue;
	processed[this] = 1;
	if (!(hlast && el[this] == "H")) {
# Defer hydrogens
	    ind++;
	    lookup[this] = ind;
	    revlookup[ind] = this;
	}
# Add all neighbors to stack in reverse order
	for (i = nbond[this]; i > 0 ; i--) {
	    n = bonded[this, i];
	    stackptr++;
	    stack[stackptr] = n;
	}
    }

    if (hlast) {
# Add hydrogens in order of heavy atoms
	oind = ind;
	for (i = 1; i <= oind; i++) {
	    j = revlookup[i];
	    for (k = 1; k <= nbond[j]; k++) {
		j1 = bonded[j, k];
		if (lookup[j1] > 0)
		    continue;
		processed[j1] = 1;
		ind++;
		lookup[j1] = ind;
		revlookup[ind] = j1;
	    }
	}
    }

# Write out atoms
    for (i = 1; i <= natoms; i++) {
        j = revlookup[i];
	if (relabel) {
# Relabel the atoms
	    count[el[j]]++;
	    name[j] = el[j] count[el[j]];
	}
	printf "ATOM  %5d  %-4s%s\n", i, name[j], data[j];
    }

# Write out bonds in lexicographic order
    bondind = 0;
    for (i = 1; i <= natoms; i++) {
	j = revlookup[i];
	for (k = 1; k <= nbond[j]; k++) { # No need to sort these bonds?
	    m = bonded[j, k];
	    i1 = lookup[m]
	    if (i1 > i) {
		bondind++;
		printf "BOND%5d%5d%5d%5d   %-4s %-4s\n",
		    bondind, i, i1, bondtype[j, m], name[j], name[m];
	    }
	}
    }
}' startatom=$startatom hlast=$hlast relabel=$relabel

