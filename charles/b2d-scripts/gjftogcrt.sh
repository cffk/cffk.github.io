#! /bin/sh

ID='$Id: gjftogms.awk 5724 2004-12-03 15:28:39Z ckarney $'

usage="
$ID
(c) 2004, Sarnoff Corporation, Princeton, NJ, USA

This shell script converts a Gaussian input file to a pure Cartesian
representation.

Run as a filter:

    $0 [-h] < input > output

Optional argument -h prints this message.

For more info see:
   http://www.gaussian.com
"

while getopts h c; do
    case $c in
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

#
# Beginning of main shell script
#

awk '
BEGIN {
    proc = 0;
    track = 0;
    comment = "";
    control = "";
    natoms = 0;
    nvars = 0;
    charge = 0;
    multiplicity = 1;
    delete line;
    delete value;
    delete id;
    delete ind;
    deg = atan2(1,1)/45;
}
{
    if (proc == 1) {
	if ($1 == "" || $1 == "Variables:")
	    proc = 2;
	else {			# Processing atoms
	    natoms++;
	    line[natoms] = $0;
	    id[natoms] = $1;
	    ind[$1] = natoms;
	}
    } else if (proc == 2) {	# Processing variables
	if ($1 == "")
	    proc = 3;
	else {
	    nvars++;
	    sub(/=/, " = ");
	    value[$1] = $3;
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
    delete x; delete y; delete z;
    x[0] = 0; y[0] = 1; z[0] = 0; # Dummy atom positions
    x[-1] = 1; y[-1] = 1; z[-1] = 0;
    for (i = 1; i <= natoms; i++) {
	$0 = line[i];

	if (NF == 1) {		# Starting position
	    x[i] = 0; y[i] = 0; z[i] = 0;

	} else if (NF == 4) {	# Cartesian line
	    x[i] = $2; y[i] = $3; z[i] = $4;

	} else if (NF == 3 || NF == 5 || NF == 7 ) { # Z-matrix line

# Look up atom indices
	    k1 = lookupatom($2);
	    k2 = lookupatom($4);
	    k3 = lookupatom($6);

# Look up values.  Note: only one level of evaluation and only var,
# -var, +var suported (Gamess supports -var)
	    b = lookupvar($3);
	    a = lookupvar($5);
	    t = lookupvar($7);

# Support initial "partial" Z-matrix entries
	    if (NF == 3) {
		k2 = i - 2; a = 90; # 2nd atom on x axis
	    }
	    if (NF <= 5) {
		k3 = i - 3; t = 0; # 3rd atom in x,y plane
	    }
	    a *= deg; t *= deg;	# Convert to radians
	    x[i] = x[k1]; y[i] = y[k1]; z[i] = z[k1];

# First reference vector
	    x1 = x[k2] - x[k1]; y1 = y[k2] - y[k1]; z1 = z[k2] - z[k1];
	    norm = sqrt(x1^2 + y1^2 + z1^2);
	    x1 /= norm; y1 /= norm; z1 /= norm;

# Second reference vector
	    x2 = x[k3] - x[k2]; y2 = y[k3] - y[k2]; z2 = z[k3] - z[k2];
	    norm = x1 * x2 + y1 * y2 + z1 * z2;	# Project into perp plane
	    x2 -= norm*x1; y2 -= norm*y1; z2 -= norm*z1;
	    norm = sqrt(x2^2 + y2^2 + z2^2); # Normalize if possible.
	    if (norm > 0) {	# Can skip if sin(a) == 0.
		x2 /= norm; y2 /= norm; z2 /= norm;
	    }

# Third reference vector
	    x3 = y1 * z2 - y2 * z1;
	    y3 = z1 * x2 - z2 * x1;
	    z3 = x1 * y2 - x2 * y1;

# Compute final position
	    x[i] += b * (cos(a) * x1 + sin(a) * (cos(t) * x2 - sin(t) * x3));
	    y[i] += b * (cos(a) * y1 + sin(a) * (cos(t) * y2 - sin(t) * y3));
	    z[i] += b * (cos(a) * z1 + sin(a) * (cos(t) * z2 - sin(t) * z3));
	}
    }
    for (i = 1; i <= natoms; i++) {
	printf "    %-6s %14.6f %14.6f %14.6f\n", id[i], x[i], y[i], z[i];
    }
    printf "\n";
}
function lookupatom(id) {
# Look up id in ind array
    if (ind[id] == "")
	return id;
    else
	return ind[id];
}
function lookupvar(var) {
# Look up var in value array
    if (var == "")
	return 0;
    s = substr(var,1,1);
    if (s ~ /[-+]/) {
	s = s 1;
	var = substr(var,2);
    } else
	s = 1;
    if (value[var] == "")
	return s * var;
    return s * value[var];
}
'
