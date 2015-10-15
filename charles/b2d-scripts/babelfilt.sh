#! /bin/sh
ID='$Id: $'
usage="
$ID

Run babel as a filter:

    $0 [babel-options] input-format output-format < in > out

E.g.,

    $0 pdb ac < mol.pdb > mol.ac

babel-options must not include either of -i, -o.

babel must be installed and be in your PATH.

For more info see:
   http://openbabel.sourceforge.net
"

args=
while test $# -gt 2; do
  case $1 in
      -i* | -o* ) echo "usage: $usage" 1>&2; exit 1;;
      * ) args="$args $1";;
  esac
  shift
done

# If more than zero arguments passed
if [ $# -ne 2 ]; then
   echo "usage: $usage" 1>&2
   exit 1
fi

# Set trap to cleanup temp directory
TEMP=
trap 'trap "" 0; test "$TEMP" && rm -rf "$TEMP"; exit 1' 1 2 3 9 15
trap            'test "$TEMP" && rm -rf "$TEMP"'            0

# Create temporary directory under system tmp directory
TEMP=`mktemp -d ${TMPDIR:-/tmp}/babelXXXXXXXX`

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

babel $args -i$1 $INPUT -o$2 $OUTPUT

cat $OUTPUT
