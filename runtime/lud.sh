#!/bin/sh
rm -f  TR1
if [ x"${1}"x = x-x -o x"${1}"x = xx ]
then
   > TR1
   exit 0
fi
data=Assembly/`basename "$1" ".txt"`.txt
if [ -f "$data" ]
then
   cp "$data" TR1
   exit 0
else
   echo `basename "$0"`: There is no such data file as "$1.txt" in the Assembly folder!
   exit 2
fi
