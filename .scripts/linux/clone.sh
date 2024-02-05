#!/usr/bin/env bash
. ./.configrc

#
# get current Git Repository of the Linux Kerbel
# Source: https://kernelnewbies.org/KernelBuild
#
echo clone current Kernel and checkout last stable

if [ ! -d kernel/ ]; then
	git clone git://git.kernel.org/pub/scm/linux/kernel/git/stable/linux-stable.git kernel
fi
pushd kernel/ || exit

echo last version: $VERSION
git switch master
git pull
git checkout linux-rolling-lts
git pull

popd || exit
