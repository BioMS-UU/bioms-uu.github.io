grep -RIn --include='*.qmd' --exclude-dir='voorbeelden' 'library(' . \
| perl -ne 'while(/library\s*\(\s*["'\'']?([^,"'\'')\s]+)["'\'']?/g){print "$1\n"}' \
| sort -u \
| awk '{printf "%s\"%s\"", (NR==1?"":","), $0} END{print ""}' \
| sed 's/^/install.packages(c(/; s/$/))/'
