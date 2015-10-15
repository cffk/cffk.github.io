#! /bin/sh
ID='$Id: gmstobcc.sh 6001 2005-06-23 15:51:30Z ckarney $'

usage="
$ID
(c) 2004, Sarnoff Corporation, Princeton, NJ, USA

This shell script allows Gamess to be used instead of divcon (or mopac)
for assigning AM1-BCC charges using Antechamber.  The input is a Gamess
output file and this is converted to an antechamber file (with atom
types, atom positions, bond types and partial charges).  Partial charges
are calculated using the AM1-BCC model and these charges are symmetrized
between equivalent atoms using chargesym.  Gamess must be run in such a
way as to compute the required AM1 charges.  Typical input to Gamess
should therefore include

 \$BASIS GBASIS=AM1 \$END

A suitable input file can be created with gjftogms -a.  Run as a filter,
for example:

    gms methanol 2>&1 | tee methanol.log | $0 > methanol.ac

Options:

    $0 [-d] [-h] [-c]

By default the charge are symmetrizing by chargesym (which must be in
your PATH).  This makes chemically equivalent atoms have the same
charge.  Optional argument -c skips this step.

Optional argument -d retains the temporary directory (under ${TMPDIR:-/tmp})
for debugging.

Optional argument -h prints this message.

gmstobcc depends upon OpenBabel and Antechamber.  These must be
installed and be in your PATH.  Furthermore environment variable
AMBERHOME must be defined for Antechamber.

For more info see:
   http://amber.scripps.edu
   http://amber.scripps.edu/antechamber/antechamber.html
   http://openbabel.sourceforge.net
   http://www.msg.ameslab.gov/GAMESS/GAMESS.html
"

DEBUG=
SYM=chargesym
while getopts cdh c; do
    case $c in
	c ) SYM=cat;;
        d ) DEBUG=y;;
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

if [ -z "$DEBUG" ]; then
   # If not debugging, set trap to cleanup temp directory
   TEMP=
   trap 'trap "" 0; test "$TEMP" && rm -rf "$TEMP"; exit 1' 1 2 3 9 15
   trap            'test "$TEMP" && rm -rf "$TEMP"'            0
fi

# Create temporary directory under system tmp directory
TEMP=`mktemp -d ${TMPDIR:-/tmp}/gtobXXXXXXXX`

# If problems...
if [ $? -ne 0 ]; then
   echo "$0: Can't create temp directory, exiting..." 1>&2
   exit 1
fi

[ "$DEBUG" ] && echo "Saving intermediate files in $TEMP" 1>&2

# Need to do this because antechamber and resp write files into the
# current directory and we don't want these to interfer with other
# invocations of these programs.
cd $TEMP

#
# Beginning of main shell script
#

GAMESSLOG="tmp.log"
AC="tmp.ac"
HIN="tmp.hin"
DIVCONOUT="divcon.out.$$"

# Copy stdin to a file
cat > $GAMESSLOG

# Do a sanity check on the input file.  Does it contain the data we need
# to calculate AM1-BCC charges?

if grep 'MOPAC CHARGES' $GAMESSLOG > /dev/null; then :; else
   echo Error: Input file cannot be converted to Antechamber format.  Input 1>&2
   echo must contain AM1 charges. 1>&2 
   exit 1
fi

# Run babel to get a HyperChem file from a gamess log file
babel -igam $GAMESSLOG -ohin $HIN | grep -v "^$" 1>&2

# Get total charge from gamess file
charge=`awk '{if (/CHARGE OF MOLECULE/) print $5}' $GAMESSLOG | tail -1`

# Make a "divcon.out" file
cat $HIN | awk ' BEGIN {
      printf " ATOMIC CHARGES:\n\n";
      printf " ATOM   ELEMENTAL     PARTIAL       PARTIAL       PARTIAL\n";
      printf "  NO.    SYMBOL      MULLIKEN        CM1           CM2\n";
      printf "                      CHARGE        CHARGE        CHARGE\n\n";
      charge = 0;
   }
   {
      if ($1 == "atom") {
         printf "%4d       %-2s  %14.5f%14.5f%14.5f\n", $2, $4, $7, $7, $7;
         charge += $7;
      }
   }
   END {
      printf "\n";
      printf " TOTAL MULLIKEN CHARGE = %.4f\n", charge;
      printf " TOTAL CM1 CHARGE = %.4f\n", charge;
      printf " TOTAL CM2 CHARGE = %.4f\n", charge;
   }' > $DIVCONOUT

# Run antechamber w/BCC model, set total charge
DIVCONOUT=$DIVCONOUT \
 antechamber -i $HIN -fi hin -o $AC -fo ac -nc "$charge" -c bcc 2>&1 |
   egrep -v "^(|Running:.*divcon)$" 1>&2

# Optionally symmetrize the charges
$SYM < $AC
