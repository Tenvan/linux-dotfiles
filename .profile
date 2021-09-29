#!/usr/bin/env bash
export DOT_PROFILE="initialised"

export SCRIPTS="$HOME/.scripts"
export EDITOR=$(which micro)
export VISUAL=$EDITOR

# Development profile
export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export JAVA_HOME="/usr/lib/jvm/java-16-openjdk"
export WORK_DIR=/media/WORKSPACE/$USER/Node/OneTime

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
if [ -d "$HOME/Android/Sdk/tools" ]; then
    export PATH="$HOME/Android/Sdk/tools:$PATH"
fi

# custom profile
file="$HOME/.profile-custom"
if [ -f "$file" ]; then
    . "$file"
fi
