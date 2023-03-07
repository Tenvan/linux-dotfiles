#!/usr/bin/env bash
#############################
# initialize a XWin session #
#############################
export DOT="$DOT;.xsession"

. ~/.scripts/defs

# merge custom resources
file="$CUSTOMS/.Xresources"
if [ -f $file ]; then
    xrdb -merge "$file"
fi

# load x-environemnt
csource "$HOME/.xprofile"

exec awesome
