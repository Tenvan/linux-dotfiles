#!/usr/bin/env bash
cd ~
git checkout dark-theme -f
xrdb -merge ~/.Xresources
