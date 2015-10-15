#! /bin/sh

ID='$Id: gjftogms.sh 6001 2005-06-23 15:51:30Z ckarney $'

usage="
$ID
(c) 2004, Sarnoff Corporation, Princeton, NJ, USA

This shell script converts a Gaussian input file to a Gamess input file.

Run as a filter:

    $0 [-a] [-r] [-o] [-n name] [-s] [-d] [-h] < input > output

where input is a Gaussian input file and output is a Gamess input file.
The script first converts the atom coordinates to pure Cartesian form
using gjftogcrt and then used antechamber to convert this to Z-matrix
form.  (The atoms are reordered with acreorder to allow the Z-matrix
form to be generated.)  The result is then formatted as a Gamess input
file.  The Z-matrix form allows Gamess to optimize the structure
efficiently using delocalized coordinates.  The order of the atoms in
the input file needs to be chemically "sensible" (i.e., following bonds)
in order that the Z-matrix makes sense.

Optional argument -a includes the Gamess options to specify AM1 model
   Hamiltonian basis set.  If this argument is omitted, lines for the
   Pople N-21G split valence basis set are appended by default.  (The
   resulting Gamess log file can then be run through gmstobcc.)

Optional argument -r includes the Gamess options to compute the
   electrostatic potentials necessary for the RESP charge model.  (The
   resulting Gamess log file can then be run through gmstoresp.)

Optional argument -o includes the Gamess options to specify geometry
   optimization using delocalized coordinates.

Optional argument -n name, where name is an arbitrary description of the
   resulting Gamess file.

Optional argument -s, skips the reordering of atoms.  This assumes that
    the atoms are already in an order compatible with the Z-matrix
    representation.

Optional argument -d retains the temporary directory (under ${TMPDIR:-/tmp}) 
   for debugging.

Optional argument -h prints this message.

gjftogms depends upon Antechamber.  Antechamber must be installed and be
in your PATH.  Furthermore environment variable AMBERHOME must be
defined for Antechamber.

For more info see:
   http://amber.scripps.edu
   http://amber.scripps.edu/antechamber/antechamber.html
   http://www.gaussian.com
   http://www.msg.ameslab.gov/GAMESS/GAMESS.html
"

am1=
DEBUG=
name=
opt=
resp=
acreorder=acreorder
while getopts adhn:ors c; do
    case $c in
        a ) am1=y;;
        d ) DEBUG=y;;
        h ) echo "usage: $usage"; exit;;
        n ) name="$OPTARG";;
        o ) opt=y;;
        r ) resp=y;;
	s ) acreorder=cat;;
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
TEMP=`mktemp -d ${TMPDIR:-/tmp}/gtogXXXXXXXX`

# If problems...
if [ $? -ne 0 ]; then
   echo "$0: Can't create temp directory, exiting..." 1>&2
   exit 1
fi

[ "$DEBUG" ] && echo "Saving intermediate files in $TEMP" 1>&2

# Need to do this because antechamber writes files into the
# current directory and we don't want these to interfer with other
# invocations of antechamber.
cd $TEMP

#
# Beginning of main shell script
#

(
   echo "% Comment";  		# Needed to make the sed stuff work below
   gjftogcrt
) > gcrt

desc="`cat gcrt | sed -e '1,/^#/d' | head -2 | tail -1`"
charge=`cat gcrt | sed -e '1,/^#/d' | head -4 | tail -1 | awk '{print $1}'`
mult=`cat gcrt | sed -e '1,/^#/d' | head -4 | tail -1 | awk '{print $2}'`
natoms=`cat gcrt | sed -e '1,/^#/d' | wc -l`
natoms=`expr $natoms - 5`
nzvar=`expr 3 '*' $natoms - 6`

if [ -z "$name" ]; then
  name="$desc"
fi

antechamber -nc $charge -fi gcrt  -i gcrt -fo ac -o ac | grep -v "^$" 1>&2
$acreorder < ac > nac
antechamber -nc $charge -fo gzmat -o zmat -fi ac -i nac | grep -v "^$" 1>&2

if [ "$opt" ]; then
  echo " \$CONTRL RUNTYP=OPTIMIZE \$END"
  echo " \$ZMAT DLC=.TRUE. AUTO=.TRUE. \$END"
else
  echo " \$CONTRL RUNTYP=ENERGY \$END"
fi

if [ "$am1" ]; then
  echo " \$BASIS GBASIS=AM1 \$END"
else
  echo " \$BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 \$END"
fi

if [ "$resp" ]; then
  echo " \$ELPOT IEPOT=1 WHERE=PDC OUTPUT=BOTH \$END"
# Use 6 points / A^2 density
  dens=`echo '0.5291772108^2*6' | bc`
  echo " \$PDC PTSEL=CONNOLLY CONSTR=NONE PTDENS=$dens \$END"
fi

echo " \$CONTRL SCFTYP=RHF EXETYP=RUN MOLPLT=.TRUE. \$END"
echo " \$STATPT NSTEP=100 \$END"
echo " \$GUESS GUESS=HUCKEL \$END"


echo " \$CONTRL COORD=ZMT NZVAR=$nzvar ICHARG=$charge MULT=$mult \$END"
echo " \$DATA"
echo "$name"
echo C1
cat zmat | gjfrenumber | sed -e '1,/^#/d' | tail +5 | grep -v '^$' |
    sed -e 's/Variables://' -e s'/^  *//'
echo " \$END"
