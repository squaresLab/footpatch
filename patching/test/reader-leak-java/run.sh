#!/bin/bash

source ../../../CONFIG

rm -rf infer-out*

$INFER/infer/bin/infer -g -- javac ReaderLeaks.java
cp -r infer-out infer-out-patches

chmod -R 755 *
