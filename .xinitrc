#!/usr/bin/env bash
#########################################
# initial environment for XWin sessions #
#########################################
export DOT="$DOT;~/.xinitrc"

# Theme Definitionen
export MAIN_THEME="Silvery-GTK"
export ICON_THEME="Papirus-Dark"
export CURSOR_THEME="Bibata-Modern-Ice"
export CURSOR_SIZE=32
export THEME_FONT="Noto Sans Medium, 12"
export SOUND_THEME="Smooth"
export KVANTUM_THEME="kvantum"
export WALLPAPER_THEME="island"

# Gtk Config
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export GTK_THEME="$MAIN_THEME"
export GDK_SCALE=1
export GDK_DPI_SCALE=1

# Qt Config
# Scale Factore
# export QT_SCALE_FACTOR=1
# export QT_AUTO_SCREEN_SET_FACTOR=1

# oder dpi setzen (nicht beides gleichzeitig)
# export QT_FONT_DPI=96

# unset QT_STYLE_OVERRIDE
export QT_QPA_PLATFORMTHEME="qt6ct"
export PLASMA_USE_QT_SCALING=1

CUSTOM="$CUSTOMS/.xinitrc"
if [ -r "$CUSTOM" ]; then
    . "$CUSTOM"
fi
