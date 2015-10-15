#! /bin/sh

ID='$Id: $'

usage="
$ID
(c) 2004, Sarnoff Corporation, Princeton, NJ, USA

This shell script renumbers the atoms in a Gaussian input file.

Run as a filter:

    $0 [-n num] [-k] [-l list] [-s suf] [-u] [-h] < input > output

where input is a Gaussian input file.  The atom labels will be labeled
using element+index.  A separate index is used for each element in the
molecule.  The variables may also be relabeled by added a suffix.  The
atom labels are used in the Z-matrix specification.  This tool enables
molecules to be glued together via simple edits of the the Gaussian
input files in Z-matrix format.

Optional argument -n sets the starting value for the index as num+1.
   (Default is num = 0 so atoms are labeled starting at 1.)

Optional argument -k means keep existing numbering (possibly incremented
   by num).

Optional argument -l provide a list of labels to use.  Implies -k.

Optional argument -s set the suffix for the variable renaming.

Optional argument -u unlabels the atoms reverting to plain element
   names.  (In this case, the atom index is used for the Z-matrix
   specification.)

Optional argument -h prints this message.

For more info see:
   http://www.gaussian.com
"

start=0
suf=
number=1
labels=
keep=0
while getopts hn:l:ks:u c; do
    case $c in
        h ) echo "usage: $usage"; exit;;
        n ) start="$OPTARG";;
        l ) labels="$OPTARG"; keep=1;;
        k ) keep=1;;
        s ) suf="$OPTARG";;
        u ) number=0;;
        * ) echo "usage: $usage" 1>&2; exit 1;;
    esac
done
shift `expr $OPTIND - 1`

# If more than zero arguments passed
if [ $# -ne 0 ]; then
   echo "usage: $usage" 1>&2
   exit 1
fi

if [ "$number" -eq 0 -a "$start" -ne 0 ]; then
   echo "$0: cannot specify -u and -s together" 1>&2; exit 1;
fi

#
# Beginning of main shell script
#

awk -v labels="$labels" '
BEGIN {
    proc = 0;
    track = 0;
    comment = "";
    control = "";
    natoms = 0;
    nvars = 0;
    delete line;
    delete vline;
    delete ind;
    delete lookup;		# lookup table for new atom names
    delete vlookup;		# lookup table for variables
    if (labels == "")
	newlab = 0;
    else {
	newlab = 1;
	gsub(/[^A-Za-z0-9]/, " ", labels);
	split(labels, lab);
    }
}
{
    if (proc == 1) {
	if ($1 == "" || $1 == "Variables:")
	    proc = 2;
	else {			# Processing atoms
	    natoms++;
	    line[natoms] = $0;
	    ind[$1] = natoms;
	    if (match($1, /[0-9]/) == 0) {
		el = $1;
		num = 0;
	    } else {
		el = substr($1, 1, match($1, /[0-9]/) - 1);
		num = substr($1,  match($1, /[0-9]/));
	    }
	    if (newlab) {
		if (match(lab[natoms], /[0-9]/) == 0)
		    el = lab[natoms];
		else if (match(lab[natoms], /[0-9]/) == 1)
		    num = lab[natoms];
		else {
		    el = substr(lab[natoms], 1,
				match(lab[natoms], /[0-9]/) - 1);
		    num = substr(lab[natoms],  match(lab[natoms], /[0-9]/));
		}
	    }
	    if (keep)
		num += start;
	    else {
		if (count[el] == "")
		    count[el] = start;
		count[el]++;
		num = count[el];
	    }
	    if (number) {
		nid[natoms] = el num; # The new id
		lookup[natoms] = nid[natoms];
		lookup[$1] = nid[natoms];
	    } else {
		nid[natoms] = el;
		lookup[natoms] = natoms;
		lookup[$1] = natoms;
	    }
	}
    } else if (proc == 2) {	# Processing variables
	if ($1 == "")
	    proc = 3;
	else {
	    nvars++;
	    sub(/=/, " = ");
	    vlookup[$1] = $1 suf;
	    vline[nvars] = vlookup[$1] "= " $3;
	}
    } else if (proc > 2) {
				# At the end; do nothing
    } else {			# Processing header
	printf "%s\n", $0;
	if (substr($1, 1, 1) == "#") { 
	    control = $0;
	    track = 1;
	} else if (track > 0) {
	    track++;
	    if (track == 3)
		comment = $0;
	    else if (track == 5) {
		charge = $1;
		multiplicity = $2;
		proc = 1;
	    }
	}
    }    
}
END {
    for (i = 1; i <= natoms; i++) {
	$0 = line[i];

	if (NF == 1) 		# Starting position
	    printf "    %-6s\n", nid[i];
	else if (NF == 4)	# Cartesian line
	    printf "    %-6s %14s %14s %14s\n", nid[i], $2, $3, $4;

	else if (NF >= 3) { # Z-matrix line
	    printf "    %-6s   %-6s %6s", nid[i], lookup[$2], lookupvar($3);
	    if (NF >= 5) {
		printf "   %-6s %6s", lookup[$4], lookupvar($5);
		if (NF >= 7)
		    printf "   %-6s %6s", lookup[$6], lookupvar($7);
	    }
	    printf "\n";
	}
    }
    if (nvars > 0) {
	printf "Variables:\n";
	for (i = 1; i <= nvars; i++)
	    printf "%s\n", vline[i];
    }
    printf "\n";
}
function lookupvar(var) {
# Look up var
    if (var == "")
	return 0;
    s = substr(var,1,1);
    if (s ~ /[-+]/) {
	var1 = substr(var,2);
    } else {
	s = "";
	var1 = var;
    }
    if (vlookup[var1] == "")
	return var;
    return s vlookup[var1];
}
' start=$start suf=$suf number=$number keep=$keep

