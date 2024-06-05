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
export PAGER="less"
export FILEMANAGER="nemo"
export BROWSER=
export COLORTERM=truecolor

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

# Conditional PATH additions
for path_candidate in \
  /opt/local/bin \
  /opt/local/sbin \
  /usr/local/bin \
  /usr/local/sbin \
  ~/.dotnet \
  ~/.dotnet/tools \
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

csource "$CUSTOMS/.profile"
csource "$HOME/.xinitrc"
csource "$HOME/.cargo/env"
