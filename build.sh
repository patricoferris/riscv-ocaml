#!/bin/bash

#./configure -no-ocamldoc -no-debugger -prefix `pwd`/install
#make -j8 world.opt
#make install
#make clean
#export PATH=`pwd`/install/bin:$PATH
#["export" "PATH=%{prefix}%/bin:$PATH"]
#echo $PATH >> `pwd`/install/bin/path.txt
#./configure --target riscv64-unknown-linux-gnu -prefix `pwd`/install -no-ocamldoc -no-debugger -target-bindir `pwd`/install/bin
make -j4 world || /bin/true
make -j4 opt
#make -j4 opt.opt
#cp `pwd`/install/bin/ocamlrun byterun
#make install
