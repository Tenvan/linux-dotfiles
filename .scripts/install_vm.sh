#!/usr/bin/env bash

sh $SCRIPTS/defs.sh

#####################
# init distro check #
#####################
PKG_FILE=pkg_to_install.txt
PKG_UNINST_FILE=pkg_to_uninstall.txt

_YAY_ALL="--needed --batchinstall --topdown --combinedupgrade \
    --nocleanmenu --devel --nodiffmenu --noeditmenu --noupgrademenu \
    --norebuild --noredownload --noprovides --pgpfetch \
    --useask --noremovemake"

YAY_ALL="--needed --batchinstall \
    --nocleanmenu --devel --nodiffmenu --noeditmenu --noupgrademenu \
    --norebuild --noredownload --noprovides --pgpfetch \
    --useask --noremovemake"

errorCheck() {
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo "abort installation script 'install_all': $1"
        exit $retVal
    fi
}

inst() {
    echo $1 >>$PKG_FILE
}

uninst() {
    echo $1 >>$PKG_UNINST_FILE
}

rm -f $PKG_FILE
rm -f $PKG_UNINST_FILE

###########################
# collect needed packages #
###########################

# .virtualbox
inst virtualbox
inst virtualbox-ext-oracle

echo "Install Manjaro Virtualbox"
inst linux$(uname -r | cut -f 1-2 -d '.' | tr -d '.')-headers
inst linux$(uname -r | cut -f 1-2 -d '.' | tr -d '.')-virtualbox-host-modules

# libvirt service and manager
inst virt-manager
inst qemu
inst qemu-arch-extra
inst libvirt

###############################
# uninstall unneeded packages #
###############################
#yay -R --noconfirm - <$PKG_UNINST_FILE
errorCheck "uninstall packages"

#################################
# install all (needed) packages #
#################################
yay -S $YAY_ALL - <$PKG_FILE
errorCheck "install packages"

## FINISHING #

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
errorCheck "libvirtd service"
