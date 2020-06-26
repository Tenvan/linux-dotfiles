#!/bin/bash

function run() {
  if ! pgrep $1; then
    $@ &
  fi
}

while pgrep -u $UID -x "picom" >/dev/null; do sleep 1; done
sh $HOME/Scripts/picom-toggle-xmonad.sh
sh $HOME/.config/polybar/launch-xmonad.sh
