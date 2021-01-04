#!/usr/bin/env bash

sh $SCRIPTS/defs.sh

#####################
# init distro check #
#####################
PKG_FILE=pkg_to_install.txt
PKG_UNINST_FILE=pkg_to_uninstall.txt

YAY_ALL="--needed --batchinstall --topdown --combinedupgrade \
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

# Manjaro
# manjaro only packages
inst manjaro-settings-samba
inst manjaro-pulse
inst manjaro-settings-manager
inst manjaro-xfce-settings

###############################################
# install wallpapers, themes, icons and fonts #
###############################################
inst manjaro-artwork
inst manjaro-artwork-extra
inst manjaro-users-artwork-wallpapers
inst manjaro-backgrounds

# icons
inst papirus-maia-icon-theme
inst vertex-maia-icon-theme
inst arc-themes
inst arc-themes-solid
inst arc-themes-solid-breath
inst arc-themes-solid-maia

inst breeze-maia-icon-themes
inst manjaro-artwork-icons
inst papirus-maia-icon-theme
inst vertex-maia-icon-theme
inst arc-maia-icon-theme
inst breath-icon-theme
inst breath2-icon-themes

###############################
# uninstall unneeded packages #
###############################
yay -R --noconfirm - <$PKG_UNINST_FILE

#################################
# install all (needed) packages #
#################################
yay -S $YAY_ALL - <$PKG_FILE
errorCheck "install packages"

## FINISHING #
