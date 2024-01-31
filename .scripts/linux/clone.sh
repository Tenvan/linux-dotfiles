#!/usr/bin/env bash
. ./.configrc

#
# get current Git Repository of the Linux Kerbel
# Source: https://kernelnewbies.org/KernelBuild
#
echo clone current Kernel and checkout last stable

git clone git://git.kernel.org/pub/scm/linux/kernel/git/stable/linux-stable.git kernel
pushd kernel/ || exit

echo last version: $VERSION
git checkout stable/linux-rolling-lts

popd || exit
