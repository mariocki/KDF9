#!/bin/bash
rm -f TR1
program=`basename "$1" ".a60"`.a60
if [ -f "Algol/$program" ]
then
   cp "Algol/$program" TR1
   exit 0
else
   echo `basename "$0"`: there is no such file as "$program" in the Algol folder!
   exit 1
fi
