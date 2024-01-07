#!/usr/bin/env bash
#############################
# initialize a XWin session #
#############################
export DOT="$DOT;~/.xsession"

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

. ~/.scripts/defs


# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# merge custom resources
file="$CUSTOMS/.Xresources"
if [ -f "$file" ]; then
    xrdb -merge "$file"
fi

#========================
# System tools autostart
#=======================

# picom starten
picom -b --dbus --config "$PICOM_CONF" &

# Polkit and keyring

# see https://wiki.archlinux.org/title/GNOME/Keyring#xinitrc
if [ $IS_ARCH_BASED = true ]; then
    /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
else
    /usr/libexec/polkit-gnome-authentication-agent-1 &
fi

eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)
export SSH_AUTH_SOCK

# load x-environemnt
csource "$HOME/.xprofile"

exec /usr/bin/awesome >> ~/.xsession-stdout 2>> ~/.xsession-stderr
