#!/bin/bash
# requires wkhtmltopdf command-line tool
# download pkg for wkhtmltopdf on website
# installed on macos m1 with following cmd:
# sudo installer -pkg wkhtmltox-0.12.6-2.macos-cocoa.pkg -target /usr/local/bin

for html in _book/Modellen/*.html; do
	filename="${html##*/}"	
	filename=${filename%%.*}
	echo wkhtmltopdf $html $filename.pdf	
done;

for html in _book/Statistiek/*.html; do
	filename="${html##*/}"	
	filename=${filename%%.*}
	echo wkhtmltopdf $html $filename.pdf	
done;
