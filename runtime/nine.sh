#!/bin/bash
cp FW0 FW0_copy

trap "cp FW0_copy FW0; exit 2" SIGQUIT SIGKILL SIGINT

HERE=`pwd`

program=`basename "$1" ".kdf9"`.kdf9
if [ ! -f Binary/"$program" ]
then
   echo `basename "$0"`: there is no program named "$program" in the Binary folder!
   exit 1
fi
if /bin/sh ./lud.sh "$2" | grep "data file"
then
   echo
   exit 2
fi

> FW0

mode=-d$3
miscellany=-m$4
if   [ "$mode" = -d ]
then
        $HERE/ee9 -sp -df "$miscellany" <Binary/"$program"  >TP0;
elif [ "$3" = - ]
then
        $HERE/ee9 -sp -df "$miscellany" <Binary/"$program"  >TP0;
elif [ "$mode" = -df ]
then
   time $HERE/ee9 -sp -df "$miscellany" <Binary/"$program"  >TP0;
else
        $HERE/ee9 -sp "$mode" "$miscellany"  <Binary/"$program"  >TP0;
fi
cp FW0 FW0_copy

if [ -s CP0 ]
then
   echo
   echo CP0:
   echo ===
   more CP0
   echo ===
fi

if [ -s TP0 ]
then
   echo
   echo TP0:
   echo ===
   more TP0
   echo ===
fi

if [ -s LP0 ]
then
   echo
   echo LP0:
   echo ===
   more LP0
   echo ===
fi
exit 0
