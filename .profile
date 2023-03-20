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
  ~/src/gocode/bin \
  ~/gocode \
  "$ANDROID_SDK_ROOT/platform-tools"
do
  if [[ -d "${path_candidate}" ]]; then
    export PATH="${PATH}:${path_candidate}"
  fi
done

csource "$CUSTOMS/.profile"
