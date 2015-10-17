#! /bin/sh
# Convert nbiblio.txt to an html page
# git log --date=short $1 | head -3 | tail -1 | tr -s ' ' '	' |
# cut -f2 | sed 's/$/./'
cat $1 |
sed -e 's/\*/<li>/' \
    -e "s%'''\([0-9][---0-9A-Z]*\)'''%<b>\1</b>%g" \
    -e "s% ''% <i>%g" -e "s%\([^ ]\)''%\1</i>%g" \
    -e "s%---%\&mdash;%g" \
    -e "s%\([^!]\)--\([^>]\)%\1\&ndash;\2%g" \
    -e "s%\([^!]\)--\([^>]\)%\1\&ndash;\2%g" \
    -e 's%doi:\([^ ]*\)%<a href="https:dx.doi.org/\1">doi:\1</a>%g' \
    -e 's%arxiv:\([^ ]*\)%<a href="http://arxiv.org/abs/\1">arxiv:\1</a>%g' \
    -e 's%inis:\([^ ]*\) \([^ ]*\)%<a href="https://inis.iaea.org/search/searchsinglerecord.aspx?recordsFor=SingleRecord\&RN=\1">\2</a>%g' \
    -e 's%patent:\([^ ]*\)%<a href="https://www.google.com/patents/\1">patent:\1</a>%g' \
    -e 's%osti:\([^ ]*\) \([^ ]*\)%<a href="http://www.osti.gov/scitech/biblio/\1">\2</a>%g'
