#!/usr/bin/env bash
# Resources for dark themes

# Personal (private) Resources
if [ -f "$HOME/.Xresources.personal" ]; then
    xrdb -merge "$HOME/.Xresources.personal"
fi
