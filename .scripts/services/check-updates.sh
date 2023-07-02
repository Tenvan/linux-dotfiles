#!/usr/bin/env bash

. ~/.scripts/defs

if [ "$IS_ARCH_BASED" = true ]; then
  pamac checkupdates
elif [ "$IS_RHEL_BASED" = true ]; then
  sudo dnf check-update -q --color false
fi
