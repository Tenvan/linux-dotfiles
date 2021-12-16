#!/usr/bin/env bash
######################
# Settings wallpaper #
######################
# WALLPAPER1=~/.local/share/wallpapers/shared/floating-world-in-space-3000x2357.jpg
# WALLPAPER2=~/.local/share/wallpapers/shared/blue-earth-2880x1800.jpg

export SHARED_WALLPAPERS="$HOME/.local/share/wallpapers/shared"
export DUAL_MONITOR_WALLPAPERS="$SHARED_WALLPAPERS/wallpapers-multimonitor"

# WALLPAPER_DUAL=dropletno7revisited.jpg
WALLPAPER_DUAL=mesh3isostudy.jpg
# WALLPAPER_DUAL=extrudedsimplicesbstudy.jpg
# WALLPAPER_DUAL=softshading.jpg
# WALLPAPER_DUAL=verticaldesireblack.jpg
# WALLPAPER_DUAL=elementalfists.jpg
# WALLPAPER_DUAL=0bonv.jpg
# WALLPAPER_DUAL=large-dual-screen-wallpaper1.jpg
# WALLPAPER_DUAL=0bonv_x_flip.jpg
# WALLPAPER_DUAL=honeycombs.jpg

export WALLPAPER_FILE=$DUAL_MONITOR_WALLPAPERS/$WALLPAPER_DUAL
######################


# nitrogen --restore
# nitrogen --set-zoom-fill $WALLPAPER1 --head=0
# nitrogen --set-zoom-fill $WALLPAPER2 --head=1
nitrogen --set-zoom-fill $WALLPAPER_FILE
