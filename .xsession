#!/usr/bin/env bash
export DOT="$DOT;.xsession"

. ~/.scripts/defs

# Load custom resources
file="$CUSTOMS/.Xresources"
if [ -f $file ]; then
    xrdb -merge "$file"
fi

# see https://unix.stackexchange.com/a/295652/332452
csource /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

# see https://wiki.archlinux.org/title/GNOME/Keyring#xinitrc
eval $(/usr/bin/gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

# see https://github.com/NixOS/nixpkgs/issues/14966#issuecomment-520083836
mkdir -p "$HOME"/.local/share/keyrings

echo "do run autostarts"

if [[ "$DESKTOP_SESSION" == "awesome" ]]; then
    "$HOME/.screenlayout/screenlayout.sh" &
    "$HOME/.bin/autostart" &    
fi

csource "$CUSTOMS/.xsession"
