#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

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
inst etcher-bin
inst gimp
inst gimp-help-de
inst gwenview
inst imagemagick
inst inkscape
inst partitionmanager
inst pinta
inst playerctl
inst pstoedit
inst python-lxml
inst python-numpy
inst teams
inst themix-full-git
inst themix-theme-arc-git 

# inst zoom
inst zoom-system-qt
inst zoom-firefox

sudo rm /var/lib/pacman/db.lck

###############################
# uninstall unneeded packages #
###############################
if [ $DEBUG != true ]; then
	eval "$PACKER -R --noconfirm $PAKAGE_UNINST"
fi

#################################
# install all (needed) packages #
#################################
if [ $DEBUG != true ]; then
	eval "$PACKER -S $PAKKU_ALL $PAKAGE_INST"
	errorCheck "install packages"
fi

## FINISHING #
if [ $ERROR_PAKAGE_UNINST ]; then
	print 'No Errors on Install'
else
	print "Error in Inst: ${ERROR_PAKAGE_INST}"
fi
