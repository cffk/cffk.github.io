#! /bin/sh
ID='$Id: $'
usage="
$ID

Run antechamber as a filter:

    $0 [-d] [-h] [antechamber-options] input-format output-format < in > out

E.g.,

    $0 pdb ac < mol.pdb > mol.ac

Optional argument -d retains the temporary directory (under ${TMPDIR:-/tmp}) 
   for debugging.

Optional argument -h prints this message.

antechamber-options must not include any of -fi, -fo, -i, -o.

Antechamber must be installed and be in your PATH.

For more info see:
   http://amber.scripps.edu
   http://amber.scripps.edu/antechamber/antechamber.html
"

args=
DEBUG=
while test $# -gt 2; do
  case $1 in
      -d ) DEBUG=y;;
      -h ) echo "usage: $usage"; exit;;
      -fi | -i | -fo | -o ) echo "usage: $usage" 1>&2; exit 1;;
      * ) args="$args $1";;
  esac
  shift
done

# If more than zero arguments passed
if [ $# -ne 2 ]; then
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
TEMP=`mktemp -d ${TMPDIR:-/tmp}/anteXXXXXXXX`

# If problems...
if [ $? -ne 0 ]; then
   echo "$0: Can't create temp directory, exiting..." 1>&2
   exit 1
fi

[ "$DEBUG" ] && echo "Saving intermediate files in $TEMP" 1>&2

cd $TEMP

INPUT=input
OUTPUT=output

cat > $INPUT

antechamber $args -fi $1 -fo $2 -i $INPUT -o $OUTPUT | grep -v "^$" 1>&2

cat $OUTPUT
