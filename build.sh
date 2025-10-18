#!/bin/sh

mkdir -p build
cd build

cmake ..
chmod u+x ./bison-shim.sh
make
