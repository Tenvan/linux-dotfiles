#!/usr/bin/env bash
#########################################
# initial environment for every session #
#########################################
export DOT="$DOT;~/.profile"

. ~/.scripts/defs

# Config Vars
export SCRIPTS="$HOME/.scripts"
export CUSTOMS="$HOME/.custom"

# System Vars
export EDITOR=micro
export VISUAL="$EDITOR"
export TERMINAL="kitty"
export TIME="/usr/bin/time -v "
export DISPLAYMANAGER=gdm
export PAGER="most"
export FILEMANAGER="nemo"
export BROWSER=

if [ -x "$(command -v fastfetch)" ]; then
	export FETCHER=fastfetch
elif [ -x "$(command -v screenfetch)" ]; then
	export FETCHER=screenfetch
fi

# Development Vars
export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export JAVA_HOME="/usr/lib/jvm/jre"
export WORKSPACE="/srv/WORKSPACE/$USER"

# Xdg Config
#export XDG_CURRENT_DESKTOP=Unity
# export XDG_CURRENT_DESKTOP=KDE

# Theme Definitionen
export MAIN_THEME="VALYRIAN-Total-Steel"
export ICON_THEME="Papirus-Dark"
export CURSOR_THEME="Bibata-Modern-Ice"
export CURSOR_SIZE=32
export THEME_FONT="Noto Sans Medium 12"
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
export QT_FONT_DPI=96

unset QT_STYLE_OVERRIDE
export QT_QPA_PLATFORMTHEME="qt6ct"
export PLASMA_USE_QT_SCALING=1

# Conditional PATH additions
for path_candidate in \
  /opt/local/bin \
  /opt/local/sbin \
  /usr/local/bin \
  /usr/local/sbin \
  ~/.cabal/bin \
  ~/.cargo/bin \
  ~/.rbenv/bin \
  ~/.bin \
  ~/.scripts \
  ~/.local/bin \
  ~/.yarn/bin \
  ~/.config/yarn/global/node_modules/.bin \
  ~/src/gocode/bin \
  ~/gocode \
  "$ANDROID_SDK_ROOT/platform-tools"
do
  if [[ -d "${path_candidate}" ]]; then
    export PATH="${PATH}:${path_candidate}"
  fi
done

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && csource "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && csource "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

csource "$CUSTOMS/.profile"
