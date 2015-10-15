#! /bin/sh -e
BASE="http://charles.karney.info/biblio"
test -d temp || mkdir temp
touch temp/junk
rm -f temp/[a-z]*
bibtex biblio
cat biblio.bbl |
sed -e 's/---/\\emdash{}/'g \
    -e 's/--/\\endash{}/g' \
    -e 's/``/\\ldquote{}/g' \
    -e "s/''/\\\\rdquote{}/g" \
 > biblio.newbbl
mv biblio.newbbl biblio.bbl
# latex biblio
pdflatex biblio
latex2html -split 0 -t 'Charles Karney: Publications' biblio
cat biblio/index.html |
sed -e '/--Navigation Panel--/,/--End of Navigation Panel/d' \
 -e '/NAME="SECTION00020000000000000000"/,/The translation was initiated by/d' \
 -e 's/<ADDRESS>/<hr><ADDRESS>/' \
 -e 's/<BODY >/<BODY topmargin=10 leftmargin=10>/' \
 -e "s%<HEAD>%<head><!--Xbase href=\"$BASE/index.html\"Y-->%" \
 -e 's%</HEAD>%</head>%' \
 -e 's/&amp;/\&/g' |
sed -e 's%<P></P><DT><A NAME="\([A-Za-z][a-z0-9]*\)">%<P></P><DT><A NAME="\1" HREF="\1.html">%g' > index.html
rm -f biblio/{index,biblio}.html

cat biblio.bib | awk 'BEGIN {
    file = 0;
}
{
    if ($1 == "")
	next;
    if (substr($1, 1, 1) == "%")
	next;
    if (substr($0, 1, 1) == "@") {
	ind = match($0, "{");
	if (ind > 0) {
	    part = substr($0, ind+1);
	    ind = match(part, ",");
	    if (ind > 1) {
		file = substr(part, 1, ind-1);
		file = "temp/" file ".bib"
	    }
	}
    }
    if (file)
	print $0 > file;
}'

num=`wc -l < temp/ent.list`
num=`echo $num`
( echo NONE
  head -`expr $num - 1` temp/ent.list
) > temp/previous.list
( tail --lines +2 temp/ent.list
  echo NONE
) > temp/next.list
grep '^{\\bibitem\[' biblio.bbl | sed -e 's/.*\[//' -e 's/\].*//' > temp/keys.list
i=0 || true
paste temp/{previous,ent,next,keys}.list |
while read prev a next key; do 
  let i=$i+1
  cat index.html |
  sed -e "1,/<P><.P><DT><A NAME=\"$a\" HREF=.*>/d" | tail --lines +2 | grep -v '<DD>' |
  sed -e "/\(<P><.P><DT><A NAME=\|<.DL>\)/,\$d" > temp/$a.entry
  if grep '\bE-print:' temp/$a.entry > /dev/null; then
    cat temp/$a.entry |
    sed -e 's%arXiv:\(.*\)</A>%& (<a href="papers/\1.pdf">Mirror</a>)%' > temp/$a.new
    mv temp/$a.new temp/$a.entry
  fi
  authors=`cat temp/$a.authors | tr -d '{}'`
  title=$(cat temp/$a.title | sed -e 's/--/-/g' -e 's/``/"/g' -e "s/''/\"/g" | tr -d "{}")

  ( cat <<EOF
<html>
<head><!--Xbase href="$BASE/$a.html"Y-->
<title>$title</title>
<meta name="description" content="`echo $title | tr -d '"'`">
<meta name="author" content="$authors">
</head>
<body topmargin=10 leftmargin=10>
<h3>Charles Karney:
Publication $i/$num, $key</h3>
EOF
if test "$prev" = NONE; then
  echo "Previous Entry"
else
  echo "<a href=\"$prev.html\">Previous Entry</a>"
fi
echo "&nbsp;&nbsp;&nbsp;&nbsp;"
echo "<a href=\"index.html#$a\">This entry</a>"
echo "in <a href=\"index.html\">bibliography</a>"
echo "&nbsp;&nbsp;&nbsp;&nbsp;"
if test "$next" = NONE; then
  echo "Next Entry"
else
  echo "<a href=\"$next.html\">Next Entry</a>"
fi
cat <<EOF
<p><hr><div align="center"><h2>$title</h2></div>
<br>
<strong>Bibliography entry</strong>
<blockquote>
EOF
  cat temp/$a.entry |
    sed -e 's/\([^0-9]\)&ndash;/\1-/g' -e 's/&ndash;\([^0-9]\)/-\1/g'
  reprint=`cat temp/$a.reprint`
  if test "$reprint"; then
  cat <<EOF
  <br>Reprint: <a href="papers/$reprint.pdf">$reprint</a>
EOF
  fi
  doi=`cat temp/$a.doi`
  if test "$doi"; then
  doiform=`echo $doi | sed -e 's/</\&lt;/g' -e 's/>/\&gt;/g'`
  cat <<EOF
  <br>DOI: <a href="http://dx.doi.org/$doi">$doiform</a>
EOF
  fi
  echo "<br><a href="$a-bib.html">BibTeX Entry</a>"
  cat <<EOF
</blockquote>
<strong>Abstract</strong>
<blockquote>
EOF
  if test `wc -w < temp/$a.abs` -gt 0; then
    cat temp/$a.abs | sed -e 's/\\%/%/g' -e 's/<su[pb]>/&<small>/g'  -e 's%</su[pb]>%</small>&%g'
  else
    echo UNAVAILABLE
  fi
  cat <<EOF
</blockquote>
<hr>
EOF
if test "$prev" = NONE; then
  echo "Previous Entry"
else
  echo "<a href=\"$prev.html\">Previous Entry</a>"
fi
echo "&nbsp;&nbsp;&nbsp;&nbsp;"
echo "<a href=\"index.html#$a\">This entry</a>"
echo "in <a href=\"index.html\">bibliography</a>"
echo "&nbsp;&nbsp;&nbsp;&nbsp;"
if test "$next" = NONE; then
  echo "Next Entry"
else
  echo "<a href=\"$next.html\">Next Entry</a>"
fi
echo "<p><address>Charles Karney</address>"
echo "</body></html>"
  ) > temp/$a.html
  (
   cat <<EOF
<html>
<head><!--Xbase href="$BASE/$a-bib.html"Y-->
<title>$title</title>
<meta name="description" content="$title">
<meta name="author" content="$authors">
</head>
<body topmargin=10 leftmargin=10>
<h3>BibTeX Entry for <a href="$a.html">$key</a></h3>
Link: <a href="$a.html">$BASE/$a.html</a>
<hr>
<font size="-1">
<pre>
EOF
   expand temp/$a.bib |
   sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'
   cat <<EOF
</pre>
</font>
<hr>
<address>Charles Karney</address>
</body></html>
EOF
  ) > temp/$a-bib.html
done
(
  cd temp
  rm -f *.{abs,authors,doi,entry,reprint,title}
  for f in [a-z]*.html; do
    test -f ../$f && cmp $f ../$f > /dev/null && rm -f $f
    test -f $f && mv -b $f ../
  done
  cd .. && rm -rf temp
)
# rsync --exclude '*~' --delete -av -e ssh ~/web/home/ petrel:/var/www/charles/
# rsync --exclude '*~' --delete -av -e ssh ~/web/ portal.pppl.gov:web/
