#!/bin/bash

source ../../../CONFIG

$INFER/infer/bin/infer -g -- javac Example.java
cp -r infer-out infer-out-patches

chmod -R 755 *
