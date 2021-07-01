#!/usr/bin/env bash
set -x

echo 'Running ~/.profile'

export SCRIPTS="$HOME/.scripts"
export WORK_DIR=/media/WORKSPACE/$USER/Node/OneTime

export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export EDITOR=$(which micro)

# fix "xdg-open fork-bomb" export your preferred browser from here
# export BROWSER=$(which firefox)
export BROWSER=

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

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
	echo "merge $file"
    . "$file"
fi
