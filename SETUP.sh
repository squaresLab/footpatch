#!/bin/bash

INFER=infer-linux64-v0.9.3

# Download infer here.
git clone https://github.com/squaresLab/infer-linux64-v0.9.3
rm -rf ./infer-linux64-v0.9.3/.git

# Setup config used by merge script
printf "#\!/bin/bash\n\nINFER="$(pwd)/${INFER}"\n" > CONFIG

# Merge footpatch
./MERGE.sh

# configure and make infer. ASSUMES OPAM SWITCH AND DEPS WORK.
cp build-infer.sh $INFER/
cd $INFER
./build-infer.sh
cd ..

# Make test so that rename is built
make -C $INFER/patching/test/null-deref-java test
