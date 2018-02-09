#!/bin/bash

# Merge footpatch implementation with v0.9.3 release

echo 'I hope you configured your infer directory in CONFIG...'

source CONFIG

cp -r patching $INFER

echo 'Copying android models'
F1=infer/models/java/src/android/database/
cp $F1/* $INFER/$F1

echo 'Copying java io models'
F2=infer/models/java/src/java/io/
cp $F2/* $INFER/$F2

echo 'Copying C io models'
F2=infer/models/c/src/
cp $F2/* $INFER/$F2

echo 'Copying backend ml files'
cp infer/src/backend/* $INFER/infer/src/backend/

echo 'Copying makefile'
cp infer/src/Makefile $INFER/infer/src/Makefile

echo 'Copying footpatch source files'
cp -r infer/src/footpatch $INFER/infer/src/

echo "Renaming infer rename command in $INFER/infer/src/footpatch/footpatch_utils.ml for your install"
# use @ delimiter because / interferes
sed -i "s@\$INFER@$INFER@" $INFER/infer/src/footpatch/rename.ml

echo "Setting rename dir for rename command"
sed -i "s@\$INFER_PLACEHOLDER@$INFER@" $INFER/patching/rename/rename

echo "Copying CONFIG"
cp CONFIG $INFER/
