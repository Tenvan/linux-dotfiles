#!/usr/bin/env bash
[[ $- != *i* ]] && return

. ~/.scripts/defs

csource "$SCRIPTS/defs.sh"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups

PS1='[\u@\h \W]\$ '

test -d "$HOME/.bin" && PATH="$HOME/.bin:$PATH"
test -d "$HOME/.local/bin" && PATH="$HOME/.local/bin:$PATH"

#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

csource ~/.aliasrc

#shopt
shopt -s autocd  # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend     # do not overwrite history
shopt -s expand_aliases # expand aliases

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex() {
  if [ -f $1 ]; then
    case $1 in
    *.tar.bz2) tar xjf $1 ;;
    *.tar.gz) tar xzf $1 ;;
    *.bz2) bunzip2 $1 ;;
    *.rar) unrar x $1 ;;
    *.gz) gunzip $1 ;;
    *.tar) tar xf $1 ;;
    *.tbz2) tar xjf $1 ;;
    *.tgz) tar xzf $1 ;;
    *.zip) unzip $1 ;;
    *.Z) uncompress $1 ;;
    *.7z) 7z x $1 ;;
    *.deb) ar x $1 ;;
    *.tar.xz) tar xf $1 ;;
    *.tar.zst) unzstd $1 ;;
    *) echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

if [ "$TERM" = "linux" ]; then
  _SEDCMD='s/.*\*color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
  for i in $(sed -n "$_SEDCMD" $HOME/.Xresources | awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}'); do
    echo -en "$i"
  done
  # clear
fi

### COLOR SCRIPTS ###
test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 3
test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 23

colors() {
  # clear
  # /opt/shell-color-scripts/colorscript.sh -e 34
  echo Script 1
  test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 1
  echo Script 8
  test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 9
  echo Script 11
  test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 11
  echo Script 10
  test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 10
  echo Script 23
  test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 23
}

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

complete -cf sudo

# Change the window title of X terminals
case ${TERM} in
	xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
		;;
	screen*)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
		;;
esac

### BASH POWERLINE ###
# eval "$(starship init bash)"

### BASH INSULTER ###
test -f /usr/share/doc/find-the-command/ftc.bash && source /usr/share/doc/find-the-command/ftc.bash

## QFC Quick Complete (Ctrl-f)
test -f /usr/share/qfc/qfc.sh && source /usr/share/qfc/qfc.sh

# HSTR configuration - add this to ~/.bashrc
alias hh=hstr                   # hh to be alias for hstr
export HSTR_CONFIG=hicolor      # get more colors
shopt -s histappend             # append new history items to .bash_history
export HISTCONTROL=ignorespace  # leading space hides commands from history
export HISTFILESIZE=10000       # increase history file size (default is 500)
export HISTSIZE=${HISTFILESIZE} # increase history size (default is 500)

# ensure synchronization between bash memory and history file
export PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"

# if this is interactive shell, then bind hstr to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* ]]; then
  bind '"\C-r": "\C-a hstr -- \C-j"'
fi

# if this is interactive shell, then bind 'kill last command' to Ctrl-x k
if [[ $- =~ .*i.* ]]; then
  bind '"\C-xk": "\C-a hstr -k \C-j"'
fi

test -f "$(which neofetch)" && $(which neofetch)

csource "$CUSTOMS/.bashrc"
