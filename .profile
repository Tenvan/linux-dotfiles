#!/usr/bin/env bash
export DOT_PROFILE="initialised"

. ~/.scripts/defs

export SCRIPTS="$HOME/.scripts"
export CUSTOMS="$HOME/.custom"
export EDITOR=$(which micro)
export VISUAL=$EDITOR
export FILEMANAGER="nemo"
export TERMINAL="kitty"
export TIME="/usr/bin/time -v "

# Development profile
export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export JAVA_HOME="/usr/lib/jvm/java-8-openjdk"
export WORKSPACE=/media/WORKSPACE/$USER

# mods korrigieren
chmod +x .bin/*
chmod +x .scripts/*

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Set $PATH if ~/.bin exist
if [ -d "$HOME/.bin" ]; then
    export PATH=$HOME/.bin:$PATH
fi

if [ -d "$HOME/.scripts" ]; then
    export PATH=$HOME/.scripts:$PATH
fi

# Set $PATH if ~/.local/bin exist
if [ -d "$HOME/.local/bin" ]; then
    export PATH=$HOME/.local/bin:$PATH
fi

# yarn bin path
if [ -d "$HOME/.yarn/bin" ]; then
    export PATH=$HOME/.yarn/bin:$PATH
fi

# add android sdk path, if installed
if [ -d "$ANDROID_SDK_ROOT/tools" ]; then
    export PATH="$HOME/tools:$ANDROID_SDK_ROOT/platform-tools:$PATH"
fi

csource "$CUSTOMS/.profile"
