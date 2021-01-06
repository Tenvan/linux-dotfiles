#!/usr/bin/env bash

echo "run: defs.sh"

#####################
# init distro check #
#####################
export LINUX_VERSION_NAME=$(lsb_release -si)
export MAKEFLAGS="-j$(nproc)"

export YAY_ALL="--needed --batchinstall --topdown --combinedupgrade \
    --nocleanmenu --devel --nodiffmenu --noeditmenu --noupgrademenu \
    --norebuild --noredownload --noprovides --pgpfetch \
    --useask --noremovemake"

export PAKKU_ALL="--color always --needed --noconfirm --verbose "
