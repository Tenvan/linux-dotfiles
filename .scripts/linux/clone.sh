#!/usr/bin/env bash
. ./.configrc

#
# get current Git Repository of the Linux Kerbel
# Source: https://kernelnewbies.org/KernelBuild
#
echo clone current Kernel and checkout last stable

git clone git://git.kernel.org/pub/scm/linux/kernel/git/stable/linux-stable.git kernel
pushd kernel/ || exit

STABLE="$(git tag -l | less)"
echo last version: $STABLE
git checkout -b stable $STABLE

popd || exit
