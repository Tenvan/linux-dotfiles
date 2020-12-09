#
# ~/.bashrc
#
echo "run: .bashrc"

source $HOME/Scripts/defs.sh

#Ibus settings if you need them
#type ibus-setup in terminal to change settings and start the daemon
#delete the hashtags of the next lines and restart
#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=dbus
#export QT_IM_MODULE=ibus

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups

PS1='[\u@\h \W]\$ '

if [ -d "$HOME/.bin" ]; then
  PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
  PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.doom.d/bin" ]; then
  PATH="$HOME/.doom.d/bin:$PATH"
fi

if [ -d "$HOME/.emacs.d/bin" ]; then
  PATH="$HOME/.emacs.d/bin:$PATH"
fi

#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

source ~/.aliasrc

#Cleanup orphaned packages
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

#get fastest mirrors in your neighborhood
# ArcoLinux
if $IS_ARCO == true; then
  alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
  alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
  alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
  alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"
fi

# Manjaro
if $IS_MANJARO == true; then
  alias mirror="sudo pacman-mirrors -i"
  alias mirrord="sudo pacman-mirrors -c Germany"
  alias mirrors="sudo pacman-mirrors -a"
fi

#shopt
shopt -s autocd  # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend     # do not overwrite history
shopt -s expand_aliases # expand aliases

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
  clear
fi

### COLOR SCRIPTS ###
test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 3
test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 23

colors() {
  clear
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

### SET VI MODE IN BASH SHELL
set -o vi

### BASH POWERLINE ###
if [ -f ~/Scripts/bash-powerline.sh ]; then
  source ~/Scripts/bash-powerline.sh
fi

### BROOT ###
if [ -f ~/.config/broot/launcher/bash/br ]; then
  source ~/.config/broot/launcher/bash/br
fi

### BASH INSULTER ###
if [ -f /usr/share/doc/find-the-command/ftc.bash ]; then
  source /usr/share/doc/find-the-command/ftc.bash
fi

## QFC Quick Complete (Ctrl-f)
if [ -f /usr/share/qfc/qfc.sh ]; then
  source /usr/share/qfc/qfc.sh
fi

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

[[ -f ~/.bashrc-personal.sh ]] && . ~/.bashrc-personal.sh

if test -f "/usr/bin/neofetch"; then
  neofetch
fi

# add android sdk path, if installed
if [ -d "$HOME/Android/Sdk/tools" ] ; then 
  PATH="$HOME/Android/Sdk/tools:$PATH"
fi
