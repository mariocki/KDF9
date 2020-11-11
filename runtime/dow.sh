#!/bin/sh
HERE=`pwd`
mode=-d$1
miscellany=-m$2
if   [ "$mode" = -d ]
then
        $HERE/ee9  <Binary/KMW0201--UPU  >TP0;
elif [ "$1" = - ]
then
        $HERE/ee9 -sp -df "$miscellany" <Binary/KMW0201--UPU  >TP0;
elif [ "$mode" = -df ]
then
   time $HERE/ee9 -sp -df "$miscellany" <Binary/KMW0201--UPU  >TP0;
else
        $HERE/ee9 -sp "$mode" "$miscellany" <Binary/KMW0201--UPU  >TP0;
fi
