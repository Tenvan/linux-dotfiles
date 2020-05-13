#!/usr/bin/env bash
if [ -f "$HOME/.Xresources.monitor" ]; then
    xrdb -merge "$HOME/.Xresources.monitor"
fi
