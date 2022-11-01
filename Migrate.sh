#!/bin/bash

cp -aR /tmp/KDF9/Documents/* Documents/

rm -rf src/*
cp -aR /tmp/KDF9/Source/* src/

git commit -a -m "Copy Docs and Source"

mv /tmp/KDF9/Testing/Adjuncts/kal3.[cy] kal3/

git commit -a -m "Copy Kal3"

make patch

git commit -a -m "Apply runtime patches"

rm -rf runtime/Assembly/*
mv /tmp/KDF9/Testing/Assembly/* runtime/Assembly/

git commit -a -m "Copy Assembly"

rm -rf runtime/Data/*
cp -aR /tmp/KDF9/Testing/Data/* runtime/Data/

git commit -a -m "Copy Data"

rm -rf runtime/Kidsgrove/*
cp -aR /tmp/KDF9/Testing/Kidsgrove/* runtime/Kidsgrove/

git commit -a -m "Copy Kidsgrove"

mkdir -p runtime/Pascal
rm -rf runtime/Pascal/*

git commit -a -m "Copy Pascal"

rm -rf runtime/Whetstone/*
cp -aR /tmp/KDF9/Testing/Whetstone/* runtime/Whetstone/

git commit -a -m "Copy Whetstone"
