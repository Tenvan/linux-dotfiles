#!/usr/bin/env bash

echo "run: defs.sh"

#####################
# init distro check #
#####################
export LINUX_VERSION_NAME=$(lsb_release -si)
export IS_ARCH=false
export IS_ARCO=false
export IS_MANJARO=false
export IS_GARUDA=false

if [ $LINUX_VERSION_NAME = "Archlinux" ]; then
	export IS_ARCH=true
fi

if [ $LINUX_VERSION_NAME = "Arcolinux" ]; then
	export IS_ARCO=true
fi

if [ $LINUX_VERSION_NAME = "Manjaro" ]; then
	export IS_MANJARO=true
fi

if [ $LINUX_VERSION_NAME = "Garuda" ]; then
	export IS_GARUDA=true
fi
echo "IsArch:    $IS_ARCH"
echo "IsArco:    $IS_ARCO"
echo "IsGaruda:  $IS_GARUDA"
echo "IsManjaro: $IS_MANJARO"

export MAKEFLAGS="-j$(nproc)"

export YAY_ALL="--needed --batchinstall --topdown --combinedupgrade \
    --nocleanmenu --devel --nodiffmenu --noeditmenu --noupgrademenu \
    --norebuild --noredownload --noprovides --pgpfetch \
    --useask --noremovemake"

export PAKKU_ALL="--color always --needed --noconfirm "
