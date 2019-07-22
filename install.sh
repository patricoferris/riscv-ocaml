#!/bin/sh -e

PREFIX="$1"
#added seq only
for pkg in bigarray bytes compiler-libs dynlink findlib graphics seq stdlib str threads unix; do
  cp -r "${OPAM_SWITCH_PREFIX}/lib/${pkg}" "${OPAM_SWITCH_PREFIX}/riscv-sysroot/lib/"
done
sed -i -e 's/unix//g' ${OPAM_SWITCH_PREFIX}/riscv-sysroot/lib/bigarray/META
ln -sf "${OPAM_SWITCH_PREFIX}/bin/ocamlrun" "${OPAM_SWITCH_PREFIX}/riscv-sysroot/bin/ocamlrun"
mkdir -p "${OPAM_SWITCH_PREFIX}/lib/findlib.conf.d"
cp riscv.conf "${OPAM_SWITCH_PREFIX}/lib/findlib.conf.d"
