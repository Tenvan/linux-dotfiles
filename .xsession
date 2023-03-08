#!/usr/bin/env bash
#############################
# initialize a XWin session #
#############################
export DOT="$DOT;.xsession"
export errfile="$HOME/.xsession-errors"

. ~/.scripts/defs

# merge custom resources
file="$CUSTOMS/.Xresources"
if [ -f $file ]; then
    xrdb -merge "$file"
fi

# load x-environemnt
csource "$HOME/.xprofile"

exec /usr/bin/awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
