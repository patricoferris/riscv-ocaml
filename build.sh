#!/bin/bash

./configure -no-ocamldoc -no-debugger -prefix `pwd`/install
make -j world.opt
make install
make clean
export PATH=`pwd`/install/bin:$PATH
./configure --target riscv64-unknown-linux-gnu -no-ocamldoc -no-debugger -target-bindir `pwd`/install/bin
make -j4 world || /bin/true
make -j4 opt
