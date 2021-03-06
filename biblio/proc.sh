#! /bin/sh
# Convert biblio.txt to an html page
# git log --date=short $1 | head -3 | tail -1 | tr -s ' ' '	' |
# cut -f2 | sed 's/$/./'
date=`find $1 -printf '%TY-%Tm-%Td'`
cat <<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
  <head>
    <title>Charles Karney: Publications</title>
    <meta name="description" content="Charles Karney: Publications">
    <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
    <link rel="stylesheet" type="text/css" href="../default.css">
    <style type="text/css"> li.biblio { margin-top: 5px; } </style>
</head>

EOF
grep -v '^ *%' $1 |
sed -e "s/@DATE@/$date/" \
    -e 's/\*/<li class=biblio>/' \
    -e "s%'''\([0-9][---0-9A-Z]*\)'''%<b>\1</b>%g" \
    -e "s% ''% <i>%g" -e "s%\([^ ]\)''%\1</i>%g" \
    -e "s%---%\&mdash;%g" -e "s%--%\&ndash;%g" \
    -e 's%doi:\([^ ]*\)%<a href="https://doi.org/\1">doi:\1</a>%g' \
    -e 's%ar[xX]iv:\([^ ]*\)%<a href="https://arxiv.org/abs/\1">arXiv:\1</a>%g' \
    -e 's%oclc:\([^ ]*\)%<a href="https://www.worldcat.org/oclc/\1">oclc:\1</a>%g' \
    -e 's%inis:\([^ ]*\) \([^ ]*\)%<a href="https://inis.iaea.org/search/search.aspx?orig_q=RN:\1">\2</a>%g' \
    -e 's%patent:\([^ ]*\)%<a href="https://www.google.com/patents/\1">patent:\1</a>%g' \
    -e 's%osti:\([^ ]*\) \([^ ]*\)%<a href="http://www.osti.gov/scitech/biblio/\1">\2</a>%g' \
    -e 's/É/\&Eacute;/g' \
    -e 's/é/\&eacute;/g' \
    -e 's/à/\&agrave;/g' \
    -e 's/è/\&egrave;/g' \
    -e 's/ù/\&ugrave;/g' \
    -e 's/ç/\&ccedil;/g' \
    -e 's/ä/\&auml;/g' \
    -e 's/Ü/\&Uuml;/g' \
    -e 's/ï/\&iuml;/g' \
    -e 's/ö/\&ouml;/g' \
    -e 's/ü/\&uuml;/g' \
    -e 's/ß/\&szlig;/g'
