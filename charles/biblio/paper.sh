i=0
for e in `cat temp/ent.list `; do
  let i=$i+1
  grep '\.pdf.' $e.html |
   sed -e 's/.*"\(.*\)".*/\1/g' -e 's%http://charles.karney.info/%../%' |
   sort -u |
   sed "s/^/ENTRY $i $e /"
done > paper.txt
