#!/usr/bin/env zsh

#####################
# init distro check #
#####################
export SCRIPTS="$HOME/.scripts"

print "Linux Version: $LINUX_VERSION_NAME"
print "IsArch:        $IS_ARCH"
print "IsArco:        $IS_ARCO"
print "IsGaruda:      $IS_GARUDA"
print "IsManjaro:     $IS_MANJARO"

export MAKEFLAGS="-j$(nproc)"
export PAKKU_ALL="--color always --needed --noconfirm "
