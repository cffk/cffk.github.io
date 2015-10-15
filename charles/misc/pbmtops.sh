#! /bin/sh

# pbmtops -- convert a PBM file to PostScript using tiff2ps.
# written by Charles Karney, http://charles.karney.info, 2005

# See
#    http://charles.karney.info/misc/bitmap.html

RES=300
while getopts r: c; do
  case $c in
    r ) RES=$OPTARG;;
    * ) echo usage: $0 [-r res] [file] 1>&2; exit 1;;
  esac
done
shift `expr $OPTIND - 1`

case $# in
  0 ) FILE=;;
  1 ) FILE=$1;;
  * ) echo usage: $0 [-r res] [file] 1>&2; exit 1;;
esac

TEMP=
trap 'trap "" 0; test "$TEMP" && rm -rf "$TEMP"; exit 1' 1 2 3 9 15
trap            'test "$TEMP" && rm -rf "$TEMP"'            0
TEMP=`mktemp -d ${TMPDIR:-/tmp}/pbmtopsXXXXXXXX`

if [ $? -ne 0 ]; then
   echo "$0: Can't create temp directory, exiting..." 1>&2
   exit 1
fi


# Possible extension:  Allow setting of black and white colors...
# Replace
#     /DeviceGray setcolorspace
# by
#     [ /Indexed /DeviceRGB 1 <black white> ] setcolorspace
# e.g., for blue text on yellow background
#     [ /Indexed /DeviceRGB 1 <0000ff ffff00> ] setcolorspace

pnmtotiff -g4 -rowsperstrip 2147483647 \
  -xresolution $RES -yresolution $RES $FILE > $TEMP/tiff

PATTERN=`echo "$TEMP/tiff" | sed -e 's%/%\\\\/%g'`
if test -z "$FILE"; then
  REPLACEMENT="(stdin)"
else
  REPLACEMENT=`echo "$FILE" | sed -e 's%/%\\\\/%g'`
fi

tiff2ps -2 -z $TEMP/tiff |
  sed -e 's/^%%Creator: tiff2ps$/%%Creator: pbmtops/' \
      -e "s/^%%Title: $PATTERN\$/%%Title: $REPLACEMENT/" | dscframe
