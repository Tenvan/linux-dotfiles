#!/usr/bin/env bash

echo "run: .profile"

export ANDROID_SDK_ROOT=$HOME/Android/Sdk
export EDITOR=$(which micro)
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_QPA_PLATFORMTHEME="qt5ct"
export SCRIPTS="$HOME/.scripts"
export TERM=alacritty

# fix "xdg-open fork-bomb" export your preferred browser from here
# export BROWSER=$(which firefox-developer-edition)

test -f "$HOME/.profile-custom" && source "$HOME/.profile-custom"

# Some Vars settings in '.profile.custom':
# export WORK_DIR=<Work Folder>
# export BW_SESSION=<Bitwarden Session Token>
