#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_apps"

#########################
# collect optional apps #
#########################

# gimicks
inst cmatrix
inst hollywood

# Office
inst libreoffice-fresh
inst libreoffice-fresh-de

# language files
inst man-pages-de
inst aspell-de
inst mythes-de
inst libmythes
inst languagetool

# Browser
inst google-chrome
inst microsoft-edge-dev-bin

# optional application packages
inst aspell
inst cpu-x
inst i-nex
inst baobab
uninst etcher-bin
inst gimp
inst gimp-help-de
inst gwenview
inst imagemagick
inst inkscape
inst partitionmanager
inst pinta
inst playerctl
inst pstoedit
uninst python-lxml
uninst python-numpy
inst teams

if [ $IS_ARCO = true ]; then
	inst themix-full
	inst themix-icons-numix
else
	inst themix-full-git
fi

###############################
# uninstall unneeded packages #
###############################
fullUninstall

#################################
# install all (needed) packages #
#################################
fullInstall

## FINISHING #
finish
