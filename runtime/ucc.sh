#!/bin/sh
name=`basename "$1" ".k3"`
if [ -f "Assembly/$name.k3" ]
then
   ./kal3 "Assembly/$name.k3" > "Assembly/$name-listing.txt"
   mv mt.out Binary/"$name.kdf9"
   rm -f a.out
   exit 0
else
   echo `basename "$0"`: there is no file named "$name.k3" in the Assembly folder!
   echo
   exit 1
fi