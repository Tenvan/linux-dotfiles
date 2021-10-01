#!/usr/bin/env bash
export DOT_XSESSION="initialised"

######################
# Settings wallpaper #
######################
# WALLPAPER1=~/.local/share/wallpapers/shared/floating-world-in-space-3000x2357.jpg
# WALLPAPER2=~/.local/share/wallpapers/shared/blue-earth-2880x1800.jpg

#WALLPAPER_DUAL=dropletno7revisited.jpg
#WALLPAPER_DUAL=mesh3isostudy.jpg
#WALLPAPER_DUAL=extrudedsimplicesbstudy.jpg
#WALLPAPER_DUAL=softshading.jpg
#WALLPAPER_DUAL=verticaldesireblack.jpg
#WALLPAPER_DUAL=honeycombs.jpg
WALLPAPER_DUAL=elementalfists.jpg
#WALLPAPER_DUAL=0bonv.jpg
#WALLPAPER_DUAL=large-dual-screen-wallpaper1.jpg
#WALLPAPER_DUAL=0bonv_x_flip.jpg

DUAL_PATH="$HOME/.local/share/wallpapers/shared/wallpapers-multimonitor"
export WALLPAPER_FILE=$DUAL_PATH/$WALLPAPER_DUAL
#
######################

# Load custom resources
file="$HOME/.Xresources-custom"
if [ -f "$file" ]; then
	echo "merge $file"
    xrdb -merge "$file"
fi

# custom session
file="$HOME/.xsession-custom"
if [ -f "$file" ]; then
    . "$file"
fi

echo "do run autostarts"
$HOME/.screenlayout/screenlayout.sh
$HOME/.scripts/autostart-awesome.sh

file=$HOME/.custom/autostart
if [ -f "$file" ]; then
    . "$file"
fi
