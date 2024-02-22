#!/usr/bin/env bash
. ./.configrc

#
# build the Linux Kerbel
# Source: https://kernelnewbies.org/KernelBuild
#
pushd kernel/ || exit

export PARAMS="-j $(( $(nproc) - 2 ))"

make clean $PARAMS
make $PARAMS
make bzImage $PARAMS
make modules $PARAMS

popd || exit
