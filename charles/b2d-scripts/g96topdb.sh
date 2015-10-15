#! /bin/sh

ID='$Id: g96topdb.sh 6001 2005-06-23 15:51:30Z ckarney $'

usage="
$ID
(c) 2004, Sarnoff Corporation, Princeton, NJ, USA

This shell script: reads in a Gromacs96 (g96) file, performs OPLS to
Amber atom type and residue translation, replaces hydrogen bond lengths
with bond lengths consistent with Amber force field, corrects for the SH
bond angle in CYS, and finally writes the result to a pdb file.  Amber
residue variants by including the variant code (ASH, CYX, GLH, HID, HIE,
or LYN) in columns 73-75 of the output.

Run as a filter, thus

    cat protein.g96 | $0 > protein.pdb

Optional argument -h prints this message.

This shell script does not depend upon or call GROMACS; however, it does
expect a Gromacs96 (g96) formatted file as input.

For more info see:
   http://www.gromacs.org
"

DEBUG=
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

awk 'BEGIN {
    processing = 0;
    chain = "A";
    rec = "ATOM";
    dist = 0;

# OPLS to Amber residue translation.  The first 3 letters of the result
# give the standard residue name.  The last 3 letters give the more
# specific residue variant.
    resv["ACE" ] = "ACEACE";
    resv["NHE" ] = "NHENHE";
    resv["NAC" ] = "NMENME";
    resv["ARG" ] = "ARGARG";
    resv["ALA" ] = "ALAALA";
    resv["ASN" ] = "ASNASN";
    resv["ASP" ] = "ASPASP";
    resv["ASPH"] = "ASPASH";
    resv["CYSH"] = "CYSCYS";
    resv["CYS2"] = "CYSCYX";
    resv["GLN" ] = "GLNGLN";
    resv["GLU" ] = "GLUGLU";
    resv["GLUH"] = "GLUGLH";
    resv["GLY" ] = "GLYGLY";
    resv["HISA"] = "HISHID";
    resv["HISB"] = "HISHIE";
    resv["HISH"] = "HISHIS";
    resv["ILE" ] = "ILEILE";
    resv["LEU" ] = "LEULEU";
    resv["LYS" ] = "LYSLYN";
    resv["LYSH"] = "LYSLYS";
    resv["MET" ] = "METMET";
    resv["PHE" ] = "PHEPHE";
    resv["PRO" ] = "PROPRO";
    resv["SER" ] = "SERSER";
    resv["THR" ] = "THRTHR";
    resv["TRP" ] = "TRPTRP";
    resv["TYR" ] = "TYRTYR";
    resv["VAL" ] = "VALVAL";

# OPLS to Amber atom translation.  Key is Amber residue variant + OPLS
#  atom name.
    name["ARGHB1" ] = "HB2";
    name["ARGHB2" ] = "HB3";
    name["ARGHD1" ] = "HD2";
    name["ARGHD2" ] = "HD3";
    name["ARGHG1" ] = "HG2";
    name["ARGHG2" ] = "HG3";
    name["ASHHB1" ] = "HB2";
    name["ASHHB2" ] = "HB3";
    name["ASNHB1" ] = "HB2";
    name["ASNHB2" ] = "HB3";
    name["ASPHB1" ] = "HB2";
    name["ASPHB2" ] = "HB3";
    name["CYSHB1" ] = "HB2";
    name["CYSHB2" ] = "HB3";
    name["CYXHB1" ] = "HB2";
    name["CYXHB2" ] = "HB3";
    name["GLHHB1" ] = "HB2";
    name["GLHHB2" ] = "HB3";
    name["GLHHG1" ] = "HG2";
    name["GLHHG2" ] = "HG3";
    name["GLNHB1" ] = "HB2";
    name["GLNHB2" ] = "HB3";
    name["GLNHG1" ] = "HG2";
    name["GLNHG2" ] = "HG3";
    name["GLUHB1" ] = "HB2";
    name["GLUHB2" ] = "HB3";
    name["GLUHG1" ] = "HG2";
    name["GLUHG2" ] = "HG3";
    name["GLYHA1" ] = "HA2";
    name["GLYHA2" ] = "HA3";
    name["HIDHB1" ] = "HB2";
    name["HIDHB2" ] = "HB3";
    name["HIEHB1" ] = "HB2";
    name["HIEHB2" ] = "HB3";
    name["HIPHB1" ] = "HB2";
    name["HIPHB2" ] = "HB3";
    name["ILECD"  ] = "CD1";
    name["ILEHD1" ] = "HD11";
    name["ILEHD2" ] = "HD12";
    name["ILEHD3" ] = "HD13";
    name["ILEHG11"] = "HG12";
    name["ILEHG12"] = "HG13";
    name["LEUHB1" ] = "HB2";
    name["LEUHB2" ] = "HB3";
    name["LYNHB1" ] = "HB2";
    name["LYNHB2" ] = "HB3";
    name["LYNHD1" ] = "HD2";
    name["LYNHD2" ] = "HD3";
    name["LYNHE1" ] = "HE2";
    name["LYNHE2" ] = "HE3";
    name["LYNHG1" ] = "HG2";
    name["LYNHG2" ] = "HG3";
    name["LYNHZ1" ] = "HZ2";
    name["LYNHZ2" ] = "HZ3";
    name["LYSHB1" ] = "HB2";
    name["LYSHB2" ] = "HB3";
    name["LYSHD1" ] = "HD2";
    name["LYSHD2" ] = "HD3";
    name["LYSHE1" ] = "HE2";
    name["LYSHE2" ] = "HE3";
    name["LYSHG1" ] = "HG2";
    name["LYSHG2" ] = "HG3";
    name["METHB1" ] = "HB2";
    name["METHB2" ] = "HB3";
    name["METHG1" ] = "HG2";
    name["METHG2" ] = "HG3";
    name["NHEH1"  ] = "HN1";
    name["NHEH2"  ] = "HN2";
    name["PHEHB1" ] = "HB2";
    name["PHEHB2" ] = "HB3";
    name["PROH1"  ] = "H2";
    name["PROH2"  ] = "H3";
    name["PROHB1" ] = "HB2";
    name["PROHB2" ] = "HB3";
    name["PROHD1" ] = "HD2";
    name["PROHD2" ] = "HD3";
    name["PROHG1" ] = "HG2";
    name["PROHG2" ] = "HG3";
    name["SERHB1" ] = "HB2";
    name["SERHB2" ] = "HB3";
    name["TRPHB1" ] = "HB2";
    name["TRPHB2" ] = "HB3";
    name["TYRHB1" ] = "HB2";
    name["TYRHB2" ] = "HB3";

# pdb2gmx uses 1 A as the heavy-hydrogen bond length.  Important to use
# lengths consistent with the Amber force field so that, e.g., hydrogen
# bonding is computed corrected.  Replace with Amber lengths from
# all_amino94.in.  These are slightly inconsistent with the AmberBond
# data (e.g., S-H length is 1.33 instead of 1.336).  Finally, pdb2gmx
# uses 109.5 deg as the angle for the SH bond in CYS.  Amber uses 96
# deg.  We correct this too.
    hbond["C"] = 1.09;
    hbond["O"] = 0.96;
    hbond["N"] = 1.01;
    hbond["S"] = 1.33;
    HSangle = 96 * atan2(1,1)/45;
}
{
    if ($0 == "POSITION") {
	processing = 1;
    } else if ($0 == "END") {
	processing = 0;
    } else if (processing) {
	seq = $1;

	res = $2;
	amberres=resv[res];	# Look up Amber residue
	if (amberres == "") {
	    resvar = res;
	} else {
	    resvar = substr(amberres, 4, 3);
	    res = substr(amberres, 1, 3);
	}

	atom = $3;
	altatom = name[resvar atom]; # Look up Amber name
	if (altatom != "")
	    atom = altatom;
	if (atom == "O1")
	    atom = "O";		# Rename Os in C terminal
	else if (atom == "O2")
	    atom = "OXT";

	num = $4;

	x = $5 * 10;		# Convert from nm to A
	y = $6 * 10;
	z = $7 * 10;

	element = substr(atom, 1, 1);

	if (element != "H") {
	    oox = ox;
	    ooy = oy;
	    ooz = oz;
	    ox = x;
	    oy = y;
	    oz = z;
	    dist = hbond[element];
	    if (dist == "")
		dist = 0;
	} else if (dist > 0) {	# Fix position of Hs
	    dx = x - ox;
	    dy = y - oy;
            dz = z - oz;
	    odist = sqrt(dx * dx + dy * dy + dz * dz);
	    if (odist > 0.9 && odist < 1.1) {
		if (!(resvar == "CYS" && atom == "HG")) {
		    scale = dist/odist;
		    x = ox + scale * dx;
		    y = oy + scale * dy;
		    z = oz + scale * dz;
		} else {
		    dx /= odist;
		    dy /= odist;
		    dz /= odist;
		    refx = oox - ox;
		    refy = ooy - oy;
		    refz = ooz - oz;
		    refd = sqrt(refx * refx + refy * refy + refz * refz);
		    if (refd > 0) {
			refx /= refd;
			refy /= refd;
			refz /= refd;
			dotprod = dx * refx + dy * refy + dz * refz;
			dx -= dotprod * refx;
			dy -= dotprod * refy;
			dz -= dotprod * refz;
			norm = sqrt(dx * dx + dy * dy + dz * dz);
			if (norm > 0) {
			    x = ox + dist * (cos(HSangle) * refx + \
					     sin(HSangle) * dx/norm);
			    y = oy + dist * (cos(HSangle) * refy + \
					     sin(HSangle) * dy/norm);
			    z = oz + dist * (cos(HSangle) * refz + \
					     sin(HSangle) * dz/norm);
			} else
			    printf "REMARK SUSPECT CYS ANGLE\n";
		    } else
			printf "REMARK SUSPECT CYS DISTANCE\n";
		}
	    } else
		printf "REMARK SUSPECT H DISTANCE\n";
	}
	
	if (atom != "N" || resvar == res || resvar == "HIP")
	    resvar = "";	# Only mark variant residues when necessary

	if (length(atom) == 4)	# Reorder characters in atom name
	    atom = substr(atom,4,1) substr(atom,1,3);
	else
	    atom = " " atom;

	printf "%-6s%5i %-4s%1s%-3s %1s%4i%1s   %8.3f%8.3f%8.3f",
	    rec, num, atom, "", res, chain, seq, "", x, y, z;
	if (resvar == "")
	    printf "\n";
	else			# Print residue variant
	    printf " %4s %8s    %-4s\n", "", "", resvar;
    }
}'

exit

Documentation on PDB atom records.

PDB ATOM record
0        1         2         3         4         5         6         7         8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
KEY     SER  AT  RES C SEQI      X       Y       Z    OCC   TEMP        SEG ELCH
ATOM   3668  CD2 LEU B 223      86.358  -8.262  18.816  1.00 44.58      R1   C
RRRRRRNNNNN AAAALRRR CSSSSI   XXXX.XXXYYYY.YYYZZZZ.ZZZOOO.OOTTT.TT      SSSSEEQQ

We redefine the fields beyond Z, e.g.,

KEY     SER  AT  RES C SEQ       X       Y       Z      ROT  CHARGE     RESVAM
ATOM    366  O   LYS A  47      -4.812  11.343  -6.235  180  -0.4157    LYN  N
RRRRRRNNNNN AAAALRRR CSSSSI   XXXX.XXXYYYY.YYYZZZZ.ZZZ RRRR QQQ.QQQQ    VVVVTT

Where ROT is an integer(4) giving the additional dihedral rotation when
adding hydrogen.

CHARGE is a Real(8.4).  Aligned so that decimal point coincides with
TEMP.

RESV is a Amber residue variant, length 4, left justified (currently all
    names are 3 letters long).  Ignored if the first letter is a digit
    (since it's probably a SEG name).

AM is Amber atom type, length 2, right justified.

Overview

The HETATM records present the atomic coordinate records for atoms
within "non-standard" groups. These records are used for water molecules
and atoms presented in HET groups.

Record Format

COLUMNS        DATA TYPE       FIELD          DEFINITION
-----------------------------------------------------------------------------
 1 -  6        Record name     "HETATM"
 7 - 11        Integer         serial        Atom serial number.
13 - 16        Atom            name          Atom name.
17             Character       altLoc        Alternate location indicator.
18 - 20        Residue name    resName       Residue name.
22             Character       chainID       Chain identifier.
23 - 26        Integer         resSeq        Residue sequence number.
27             AChar           iCode         Code for insertion of residues.
31 - 38        Real(8.3)       x             Orthogonal coordinates for X.
39 - 46        Real(8.3)       y             Orthogonal coordinates for Y.
47 - 54        Real(8.3)       z             Orthogonal coordinates for Z.
55 - 60        Real(6.2)       occupancy     Occupancy.
61 - 66        Real(6.2)       tempFactor    Temperature factor.
73 - 76        LString(4)      segID         Segment identifier;
                                             left-justified.
77 - 78        LString(2)      element       Element symbol; right-justified.
79 - 80        LString(2)      charge        Charge on the atom.

ATOM

The ATOM records present the atomic coordinates for standard
residues. They also present the occupancy and temperature factor for
each atom. Heterogen coordinates use the HETATM record type. The element
symbol is always present on each ATOM record; segment identifier and
charge are optional.

Record Format

COLUMNS        DATA TYPE       FIELD         DEFINITION
-----------------------------------------------------------------------------
 1 -  6        Record name     "ATOM  "
 7 - 11        Integer         serial        Atom serial number.
13 - 16        Atom            name          Atom name.
17             Character       altLoc        Alternate location indicator.
18 - 20        Residue name    resName       Residue name.
22             Character       chainID       Chain identifier.
23 - 26        Integer         resSeq        Residue sequence number.
27             AChar           iCode         Code for insertion of residues.
31 - 38        Real(8.3)       x             Orthogonal coordinates for X in
                                             Angstroms.
39 - 46        Real(8.3)       y             Orthogonal coordinates for Y in
                                             Angstroms.
47 - 54        Real(8.3)       z             Orthogonal coordinates for Z in
                                             Angstroms.
55 - 60        Real(6.2)       occupancy     Occupancy.
61 - 66        Real(6.2)       tempFactor    Temperature factor.
73 - 76        LString(4)      segID         Segment identifier, left-justified.
77 - 78        LString(2)      element       Element symbol, right-justified.
79 - 80        LString(2)      charge        Charge on the atom.
