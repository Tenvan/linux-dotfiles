#!/usr/bin/env zsh

. ~/.scripts/defs.zsh

# Init Install
initInstall "install_rust_apps"

#####################
# collect rust apps #
#####################

inst bat
inst fd
inst ripgrep
inst tokei
#inst procs

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
