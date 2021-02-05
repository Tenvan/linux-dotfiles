#!/bin/bash
echo 'Running ~/.profile'

ibus-daemon -d -x

userresources=$HOME/.Xresources-custom
userprofile=$HOME/.profile-custom

if [ -f "$userresources" ]; then
	echo "Merge $userresources"
	xrdb -merge "$userresources"
fi

export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export EDITOR=$(which micro)
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export MAIL=$(which thunderbird)
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_QPA_PLATFORMTHEME="qt5ct"
export SCRIPTS="$HOME/.scripts"

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Set $PATH if ~/.local/bin exist
if [ -d "$HOME/.local/bin" ]; then
    export PATH=$HOME/.local/bin:$PATH
fi
# add android sdk path, if installed
if [ -d "$HOME/Android/Sdk/tools" ]; then
    export PATH="$HOME/Android/Sdk/tools:$PATH"
fi

# fix "xdg-open fork-bomb" export your preferred browser from here
# export BROWSER=$(which firefox)

if [ -f "$userprofile" ]; then
	echo "Source $userprofile"
	. "$userresources"
fi
