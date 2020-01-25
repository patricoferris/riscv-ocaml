#!/bin/sh

echo "<><><><><><><><><><>Building compiler for host (x86...) with no optimisations<><><><><><><><><><><><><><><>"
make clean
git checkout 88b3b40c87f7d2c239620a04bdf715ab3abdf7c3  
./configure -no-ocamldoc -no-debugger -prefix /riscv-ocaml && make -j8 world.opt && make install 
export PATH="/riscv-ocaml/bin:${PATH}"
# Checkout the latest code with the inline assembly 
echo "Building compiler for target (riscv64-unknown-linux-gnu) with optimisations - using commit hash supplied"
git checkout $1 
make clean && ./configure --target riscv64-unknown-linux-gnu -prefix /riscv-ocaml -no-ocamldoc -no-debugger -target-bindir /riscv-ocaml/bin && make -j8 world || /bin/true 
make -j8 opt
cp /riscv-ocaml/bin/ocamlrun byterun
make install
