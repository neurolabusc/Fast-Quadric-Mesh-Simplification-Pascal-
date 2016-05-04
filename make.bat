#!/bin/sh
# change to working directory to location of command file: http://hints.macworld.com/article.php?story=20041217111834902
here="`dirname \"$0\"`"
cd "$here" || exit 1

fpc -O3 -XX -Xs simplify.pas
strip ./simplify
rm  -r *.o
rm  -r *.ppu
rm -r *.bak


