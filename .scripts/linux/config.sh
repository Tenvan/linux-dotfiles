#!/usr/bin/env bash
. ./.configrc

#
# get and edit kernel config
# Source: https://kernelnewbies.org/KernelBuild
#
pushd kernel/ || exit

if [ ! -f .config ]
    cp /boot/config-`uname -r`* .config
fi

make nconfig

popd || exit
