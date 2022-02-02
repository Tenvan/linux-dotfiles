#!/usr/bin/env zsh

. ~/.scripts/defs

# Use powerline
USE_POWERLINE="true"
# Source manjaro-zsh-configuration
if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
  source /usr/share/zsh/manjaro-zsh-config
fi
# Use manjaro zsh prompt
if [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
  source /usr/share/zsh/manjaro-zsh-prompt
fi

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/z-shell/zinit/main/doc/install.sh)"
fi

csource "$HOME/.zinit/bin/zinit.zsh"

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

zinit wait lucid for \
  atinit"zicompinit; zicdreplay" \
	zdharma-continuum/fast-syntax-highlighting \
	zdharma-continuum/history-search-multi-word \
  atload"_zsh_autosuggest_start" \
      zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
      zsh-users/zsh-completions

zinit ice depth=1; zinit light romkatv/powerlevel10k
           
### End of Zinit's installer chunk

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
csource "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"

if [ -f "$(which neofetch)" ]; then
	$(which neofetch)
fi

## Options section
setopt correct                                                  # Auto correct mistakes
setopt extendedglob                                             # Extended globbing. Allows using regular expressions with *
setopt nocaseglob                                               # Case insensitive globbing
setopt rcexpandparam                                            # Array expension with parameters
setopt nocheckjobs                                              # Don't warn about running processes when exiting
setopt numericglobsort                                          # Sort filenames numerically when it makes sense
setopt nobeep                                                   # No beep
setopt appendhistory                                            # Immediately append history instead of overwriting
setopt histignorealldups                                        # If a new command is a duplicate, remove the older one
setopt auto_pushd
# Set some options about directories
setopt pushd_ignore_dups
setopt pushdminus

setopt autocd                                                   # if only directory path is entered, cd there.
setopt AUTO_CD  # If a command is issued that can’t be executed as a normal command,
                # and the command is the name of a directory, perform the cd command
                # to that directory.

# Add some completions settings
setopt ALWAYS_TO_END     # Move cursor to the end of a completed word.
setopt AUTO_LIST         # Automatically list choices on ambiguous completion.
setopt AUTO_MENU         # Show completion menu on a successive tab press.
setopt AUTO_PARAM_SLASH  # If completed parameter is a directory, add a trailing slash.
setopt COMPLETE_IN_WORD  # Complete from both ends of a word.
unsetopt MENU_COMPLETE   # Do not autoselect the first completion entry.

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' rehash true                              # automatically find new executables in path 
zstyle ':completion:*' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)*==34=34}:${(s.:.)LS_COLORS}")'  # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*:descriptions' format '%U%F{cyan}%d%f%u'

# Load any custom zsh completions we've installed
if [[ -d ~/.zsh-completions ]]; then
  for completion in ~/.zsh-completions/*
  do
    if [[ -r "$completion" ]]; then
      source "$completion"
    else
      echo "Can't read $completion"
    fi
  done
fi

# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zcache

# Miscellaneous settings
setopt INTERACTIVE_COMMENTS  # Enable comments in interactive shell.
setopt extended_glob # Enable more powerful glob features

# automatically load bash completion functions
autoload -U +X bashcompinit && bashcompinit

# configure history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
setopt INC_APPEND_HISTORY
unsetopt HIST_BEEP

# Share your history across all your terminal windows
setopt share_history

HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=100000

export HISTIGNORE="ll:ls:cd:cd -:pwd:exit:date:* --help"

# Long running processes should return time after they complete. Specified
# in seconds.
REPORTTIME=2
TIMEFMT="%U user %S system %P cpu %*Es total"

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start { echoti smkx }
	function zle_application_mode_stop { echoti rmkx }
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

# grc colorizes the output of a lot of commands. If the user installed it,
# use it.

# Try and find the grc setup file
if (( $+commands[grc] )); then
  GRC_SETUP='/usr/local/etc/grc.bashrc'
fi
if (( $+commands[grc] )) && (( $+commands[brew] ))
then
  GRC_SETUP="$(brew --prefix)/etc/grc.bashrc"
fi
if [[ -r "$GRC_SETUP" ]]; then
  source "$GRC_SETUP"
fi
unset GRC_SETUP

if (( $+commands[grc] ))
then
  function ping5(){
    grc --color=auto ping -c 5 "$@"
  }
else
  alias ping5='ping -c 5'
fi

# These need to be done after $PATH is set up so we can find
# grc and exa

# Set up colorized ls when gls is present - it's installed by grc
# shellcheck disable=SC2154
if (( $+commands[gls] )); then
  alias ls="gls -F --color"
  alias l="gls -lAh --color"
  alias ll="gls -l --color"
  alias la='gls -A --color'
fi

# When present, use exa instead of ls
if (( $+commands[exa] )); then
  if [[ -z "$EXA_TREE_IGNORE" ]]; then
    EXA_TREE_IGNORE=".cache|cache|node_modules|vendor|.git"
  fi
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
case ${TERM} in
	xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
  		print "Init Powershell10k for XWindows"
		  csource ~/.bin/.p10k-x.zsh
    ;;
    *)
  		print "Init Powershell10k for vconsole"
      csource ~/.bin/.p10k-v.zsh
	  csource "$HOME/.profile"
    ;;
esac

csource "$HOME/.aliasrc"
csource "$CUSTOMS/.zshrc"

colorscript -e six
colorscript -e hex
