#!/usr/bin/env bash
export DOT="$DOT;.profile"
echo PROFILE: $DOT

export SCRIPTS="$HOME/.scripts"
export CUSTOMS="$HOME/.custom"
export EDITOR=micro
# export PAGER=mypager
export VISUAL="$EDITOR"
export FILEMANAGER="nemo"
export TERMINAL="kitty"
export TIME="/usr/bin/time -v "
export DISPLAYMANAGER=sddm

# Development profile
export ANDROID_SDK_ROOT="$HOME/Android/Sdk"
export JAVA_HOME="/usr/lib/jvm/java-11-openjdk"
export WORKSPACE=/media/WORKSPACE/$USER

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

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

if [ -x "$HOME/.customs/.profile" ]; then
	source "$HOME/.customs/.profile"
fi
