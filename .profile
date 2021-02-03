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

# Some Vars settings in '.profile.custom':
# export WORK_DIR=<Work Folder>
# export BW_SESSION=<Bitwarden Session Token>

# Base16 oomox-Numix-Dark-Manjaro
# Author: oomox-Numix-Dark-Manjaro

_gen_fzf_default_opts() {

local color00='#303030'
local color01='#2e585a'
local color02='#3e686a'
local color03='#949494'
local color04='#1c1c1c'
local color05='#fafafa'
local color06='#7fa9ab'
local color07='#f6ffff'
local color08='#b14a4c'
local color09='#dab159'
local color0A='#c09943'
local color0B='#2a885e'
local color0C='#44acb4'
local color0D='#5082b9'
local color0E='#a78ba9'
local color0F='#b1693e'

export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"\
" --color=bg+:$color01,bg:$color00,spinner:$color0C,hl:$color0D"\
" --color=fg:$color04,header:$color0D,info:$color0A,pointer:$color0C"\
" --color=marker:$color0C,fg+:$color06,prompt:$color0A,hl+:$color0D"

}

_gen_fzf_default_opts
