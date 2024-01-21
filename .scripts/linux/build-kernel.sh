#!/usr/bin/env bash
. ./.configrc

pushd kernel/ || exit

export PARAMS="-j $(( $(nproc) - 1 ))"

make clean $PARAMS
make $PARAMS
make bzImage $PARAMS
make modules $PARAMS

popd || exit
