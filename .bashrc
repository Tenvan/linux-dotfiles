#!/usr/bin/env bash
######################################
# initial environment for bash shell #
######################################
export DOT="$DOT;~/.bashrc"

. ~/.scripts/defs

if [[ -n $SSH_LOGIN || -z $ENV ]]; then
     # Put here login initialization code
     unset SSH_LOGIN
     ENV=~/.profile
     csource ~/.profile
fi

export HISTCONTROL=ignoreboth:erasedups

#Ibus settings if you need them
#type ibus-setup in terminal to change settings and start the daemon
#delete the hashtags of the next lines and restart
#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=dbus
#export QT_IM_MODULE=ibus

PS1='[\u@\h \W]\$ '

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

# ░█▀█░█░░░▀█▀░█▀█░█▀▀
# ░█▀█░█░░░░█░░█▀█░▀▀█
# ░▀░▀░▀▀▀░▀▀▀░▀░▀░▀▀▀
csource "$HOME/.aliasrc"

#shopt
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   tar xf $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# ░█▀█░█░█░░░░░█▄█░█░█░░░░░█▀█░█▀█░█▀▀░█░█
# ░█░█░█▀█░▄▄▄░█░█░░█░░▄▄▄░█▀▀░█░█░▀▀█░█▀█
# ░▀▀▀░▀░▀░░░░░▀░▀░░▀░░░░░░▀░░░▀▀▀░▀▀▀░▀░▀

case ${TERM} in
    xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
        echo "Init Oh-My-Posh for XWindows"
        #print "Init Powershell10k for XWindows"
        eval "$(oh-my-posh init bash --config ~/.config/powershell/xsession.omp.jsonc)"
    ;;
    *)
        echo "Init Oh-My-Posh for vconsole"
        eval "$(oh-my-posh init bash --config ~/.config/powershell/vconsole.omp.jsonc)"
        csource ~/.profile
    ;;
esac

# reporting tools - install when not installed
if [ -x "$(command -v $FETCHER)" ]; then
    eval $FETCHER
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
. "$HOME/.cargo/env"
