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
inst gimp
inst gimp-help-de
inst gwenview
inst imagemagick-full
inst inkscape
inst partitionmanager
inst pinta
inst playerctl
inst pstoedit
inst psst-git
inst teams
inst teams-insiders

inst glfw-x11
inst phonon-qt5-vlc

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

# Fix Teams Black Screen
sudo ln -f -s /bin/true /usr/share/teams/resources/app.asar.unpacked/node_modules/slimcore/bin/rect-overlay
sudo ln -f -s /bin/true /usr/share/teams-insiders/resources/app.asar.unpacked/node_modules/slimcore/bin/rect-overlay
