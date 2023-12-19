#!/usr/bin/env bash
#########################################
# initial environment for every session #
#########################################
export DOT="$DOT;.profile"

. ~/.scripts/defs

export SCRIPTS="$HOME/.scripts"
export CUSTOMS="$HOME/.custom"
export EDITOR=micro
export VISUAL="$EDITOR"
export TERMINAL="kitty"
export TIME="/usr/bin/time -v "
export DISPLAYMANAGER=gdm
export PAGER="most"

if [ -x "$(command -v fastfetch)" ]; then
	export FETCHER=fastfetch
elif [ -x "$(command -v screenfetch)" ]; then
	export FETCHER=screenfetch
fi

# Development profile
export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export JAVA_HOME="/usr/lib/jvm/jre"
export WORKSPACE="/srv/WORKSPACE/$USER"

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
