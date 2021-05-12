#!/usr/bin/env bash
echo 'Running ~/.profile'

# for qt5 apps
# export QT_STYLE_OVERRIDE=GTK+
export QT_QPA_PLATFORMTHEME=qt5ct

export SCRIPTS="$HOME/.scripts"
export LINEHEIGHT=28
export LINEOFFSET=134
export WORK_DIR=/media/WORKSPACE/$USER/Node/OneTime

export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export EDITOR=$(which micro)
export MAIL=$(which thunderbird)
export SSH_ASKPASS=$(which lxqt-openssh-askpass)

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

