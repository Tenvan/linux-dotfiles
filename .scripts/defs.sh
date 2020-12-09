#!/usr/bin/env bash

echo "run: defs.sh"

#####################
# init distro check #
#####################
export LINUX_VERSION_NAME=$(lsb_release -si)
export MAKEFLAGS="-j$(nproc)"
