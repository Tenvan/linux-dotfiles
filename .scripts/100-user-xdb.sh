#!/usr/bin/env bash
# Resources for dark themes
if [ -f "$HOME/.Xresources.theme" ]; then
    xrdb -merge "$HOME/.Xresources.theme"
fi

# Personal (private) Resources
if [ -f "$HOME/.Xresources.personal" ]; then
    xrdb -merge "$HOME/.Xresources.personal"
fi

if [ -f "$HOME/.Xresources.monitor" ]; then
    xrdb -merge "$HOME/.Xresources.monitor"
fi

export userdbloaded=true
