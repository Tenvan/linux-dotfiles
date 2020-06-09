#!/usr/bin/env bash

#######################################
# Installing all used Window Managers #
#######################################

# xmonad
yay -Syy --noconfirm --needed \
  xmonad xmonad-contrib xmonad-log xmonad-utils haskell-dbus haskell-language-server-git gmrun stack hoogle

hoogle generate

# awesome
yay -S --needed --noconfirm \
  awesome xorg-server-xephyr luacheck luarocks lua-luajson lua-socket awmtt

# i3
yay -S --noconfirm --needed \
  perl-anyevent-i3 dunst i3exit conky-i3 python-i3ipc xkb-switch-i3

# qtile
yay -S --noconfirm --needed qtile

# dwm
yay -S --noconfirm --needed dwm

# bspwm
yay -S --noconfirm --needed bspwm-manjaro

# current used settings
yay -S --noconfirm --needed \
  manjaro-xfce-settings
# manjaro-awesome-settings
