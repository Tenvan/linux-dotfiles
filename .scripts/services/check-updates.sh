#!/usr/bin/env bash

. ~/.scripts/defs

if [ $IS_ARCH_BASED = true ]; then
  pamac checkupdates
elif [ $IS_FEDORA_BASED = true ]; then
  sudo dnf check-update -q --color false
fi
