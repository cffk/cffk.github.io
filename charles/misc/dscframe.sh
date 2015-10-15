#! /bin/sh

# dscframe -- frame images with DSC comments
# written by Charles Karney, http://charles.karney.info, 2005

# See
#    http://charles.karney.info/misc/bitmap.html

# Look for images in PS file and enclose with %%BeginData / %%EndData
# Assumes images start with "exec" or "image" and are coded in ASCII85
# and so end with "~>".  Data sizes are given in "ASCII Lines".  Assume
# existing %%BeginData / %%EndData blocks measure sizes in "Lines".

case $# in
  0 ) FILE=;;
  1 ) FILE=$1;;
  * ) echo usage: [file] 1>&2; exit 1;;
esac

cat $FILE | awk '
BEGIN {
  image = 0;			# Reading existing image block
  save = 0;			# Absorbing new image block
  delete lines;
}
{
  if (image) {
      print $0;
      image--;
      if (image == 0 && $0 != "%%EndData")
	  print "% Matching EndData not found";
  } else if (save) {
      save++;
      lines[save] = $0;
      if (match($0, "~>") > 0) { # Assume this is not broken across lines
	  printf "%%%%BeginData: %i ASCII Lines\n", save;
	  for (i = 1; i <= save; i++)
	      print lines[i];
	  print "%%EndData";
	  save = 0;
	  delete line;
      }
  } else {
      if (substr($0, 1, 1) == "%" &&
	  $1 == "%%BeginData:" && $2 > 0 && $4 == "Lines") {
	  print $0;
	  image = $2 + 1;	# Add 1 for %%EndData
      } else if ($0 == "exec" || $0 == "image") {
	  save = 1;
	  lines[save] = $0;
      } else
	  print $0;
  }
}
END {
    if (image) {
	printf "%% Still expecting %i image lines", image;
    }
    if (save) {
	printf "%%%%BeginData: %i ASCII Lines\n", save;
	for (i = 1; i <= save; i++)
	    print lines[i];
	print "%%EndData";
	print "% Premature end of image data?"
	save = 0;
	delete line;
    }
}'
