#!/usr/bin/env bash
export SCRIPTS="$HOME/.scripts"

# mods korrigieren
if [ -r "$HOME/.bin" ]; then
  chmod +x $HOME/.bin/*
fi
if [ -r "$SCRIPTS" ]; then
  chmod +x $SCRIPTS/*
fi

. "$SCRIPTS/defs"

export CUSTOMS="$HOME/.custom"
export EDITOR=micro
# export PAGER=mypager
export VISUAL="$EDITOR"
export FILEMANAGER="nemo"
export TERMINAL="kitty"
export TIME="/usr/bin/time -v "

# Development profile
export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export JAVA_HOME="/usr/lib/jvm/java-8-openjdk"
export WORKSPACE=/media/WORKSPACE/$USER

# Conditional PATH additions
for path_candidate in /Applications/Xcode.app/Contents/Developer/usr/bin \
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
