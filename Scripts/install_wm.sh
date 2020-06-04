#!/usr/bin/env bash

#######################################
# Installing all used Window Managers #
#######################################

yay -Syy

# xmonad
yay -S --needed --noconfirm xmonad xmonad-contrib xmonad-log haskell-dbus gmrun stylish-haskell hlint hoogle
hoogle generate

# awesome
yay -S --needed --noconfirm awesome manjaro-awesome-settings xorg-server-xephyr luacheck luarocks lua-luajson lua-socket awmtt

# i3
yay -S --needed --noconfirm perl-anyevent-i3 dunst i3exit conky-i3 python-i3ipc xkb-switch-i3

# current used settings
yay -S --needed --noconfirm manjaro-awesome-settings
