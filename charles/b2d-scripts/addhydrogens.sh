#! /bin/sh

ID='$Id: addhydrogens.sh 6001 2005-06-23 15:51:30Z ckarney $'

usage="
$ID
(c) 2004, Sarnoff Corporation, Princeton, NJ, USA

This shell script invokes the Gromacs tool, pdb2gmx, to add hydrogens to
a pdb file produces a g96 file.  A separate script g96topdb is called to
convert the result back to a pdb file using AMBER conventions for atom
names, bond lengths, and bond angles.  This only knows how to deal with
the protein atoms in a pdb file, so you probably want to invoke it only
on the ATOM lines.

Run as a filter, thus

    grep '^ATOM  ' protein.pdb | $0 > proteinH.pdb

Optional argument -d retains the temporary directory (under ${TMPDIR:-/tmp})
for debugging.

Optional argument -h prints this message.

addhydrogens depends upon GROMACS.  Specifically pdb2gmx must be
installed and be in your PATH.

For more info see:
   http://www.gromacs.org
"

DEBUG=
while getopts dh c; do
    case $c in
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
TEMP=`mktemp -d ${TMPDIR:-/tmp}/addhXXXXXXXX`

# If problems...
if [ $? -ne 0 ]; then
   echo "$0: Can't create temp directory, exiting..." 1>&2
   exit 1
fi

[ "$DEBUG" ] && echo "Saving intermediate files in $TEMP" 1>&2

# Need to do this because programs we are dependant upon may
# write files into the current directory and we don't want these
# to interfer with other invocations of these programs.
cd $TEMP

#
# Beginning of main shell script
#

cat > protein.pdb

# Invoke pdb2bms using OPLS-AA/L all-atom force field
pdb2gmx -ff oplsaa -f protein.pdb -o protein.g96 "$@" 1>&2
g96topdb < protein.g96
