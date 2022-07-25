#!/usr/bin/env zsh
export DOT="$DOT;.zshrc"

. ~/.scripts/defs
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

# Use powerline
USE_POWERLINE="true"

# Source manjaro-zsh-configuration
# csource /usr/share/zsh/manjaro-zsh-config

# Use manjaro zsh prompt
# csource /usr/share/zsh/manjaro-zsh-prompt

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
csource "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"

# LS_COLORS
csource /usr/share/LS_COLORS/dircolors.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# ░▀█▀░█▀▀░█▀▄░█▄█░▀█▀░█▀█░█▀█░█░░░░░█▀▀░█▄█░█░█░█░░░█▀█░▀█▀░█▀█░█▀▄
# ░░█░░█▀▀░█▀▄░█░█░░█░░█░█░█▀█░█░░░░░█▀▀░█░█░█░█░█░░░█▀█░░█░░█░█░█▀▄
# ░░▀░░▀▀▀░▀░▀░▀░▀░▀▀▀░▀░▀░▀░▀░▀▀▀░░░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀░▀

# Get Terminal Emulator
TERM_EMULATOR=$(ps -h -o comm -p $PPID)

# ░█▀█░█▀▀░█▀█░█▀▀░█▀▀░▀█▀░█▀▀░█░█
# ░█░█░█▀▀░█░█░█▀▀░█▀▀░░█░░█░░░█▀█
# ░▀░▀░▀▀▀░▀▀▀░▀░░░▀▀▀░░▀░░▀▀▀░▀░▀

if [ -f "$(which neofetch)" ]; then
    if [[ "$TERM_EMULATOR" == *"kitty"* ]];
    then
        # kitty
        neofetch --backend 'kitty'
    elif [[  "$TERM_EMULATOR" == *"tmux"*  ]] || [[ "$TERM_EMULATOR" == "login" ]];
    then
        # tmux
        neofetch --backend 'w3m' --ascii_distro 'arch_small'
    else
        # xterm and rxvt
        neofetch --backend 'w3m' --xoffset 40 --yoffset 40 --gap 0
    fi
fi

# ░█░░░█▀█░█▀▀░█▀█░█░░░█▀▀
# ░█░░░█░█░█░░░█▀█░█░░░█▀▀
# ░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀▀▀

export LANG="de_DE.UTF-8"
export LC_ALL="de_DE.UTF-8"

# ░▀▀█░█▀▀░█░█░░░█▀▀░█▀█░█▀█░█▀▀░▀█▀░█▀▀
# ░▄▀░░▀▀█░█▀█░░░█░░░█░█░█░█░█▀▀░░█░░█░█
# ░▀▀▀░▀▀▀░▀░▀░░░▀▀▀░▀▀▀░▀░▀░▀░░░▀▀▀░▀▀▀

# Improved less option
export LESS="--tabs=4 --no-init --LONG-PROMPT --ignore-case --quit-if-one-screen --RAW-CONTROL-CHARS"

# Watching other users
# WATCHFMT="%n %a %l from %m at %t."
watch=(notme)				# Report login/logout events for everybody except ourself.
# watch=(all)					# Report login/logout events for everybody except ourself.
LOGCHECK=60				# Time (seconds) between checks for login/logout activity.
REPORTTIME=5				# Display usage statistics for commands running > 5 sec.
# WORDCHARS="\"*?_-.[]~=/&;!#$%^(){}<>\""
# WORDCHARS="\"*?_-[]~&;!#$%^(){}<>\""
WORDCHARS='`~!@#$%^&*()-_=+[{]}\|;:",<.>/?'"'"

# ░█░█░▀█▀░█▀▀░▀█▀░█▀█░█▀▄░█░█
# ░█▀█░░█░░▀▀█░░█░░█░█░█▀▄░░█░
# ░▀░▀░▀▀▀░▀▀▀░░▀░░▀▀▀░▀░▀░░▀░
HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=100000

setopt autocd							# Allow changing directories without `cd`
setopt append_history				# Dont overwrite history
setopt extended_history			# Also record time and duration of commands.
setopt share_history				# Share history between multiple shells
setopt hist_expire_dups_first	# Clear duplicates when trimming internal hist.
setopt hist_find_no_dups			# Dont display duplicates during searches.
setopt hist_ignore_all_dups		# Remember only one unique copy of the command.
setopt hist_ignore_dups			# Ignore consecutive duplicates.
setopt hist_ignore_space
setopt hist_reduce_blanks		# Remove superfluous blanks.
setopt hist_save_no_dups		# Omit older commands in favor of newer ones.
setopt hist_verify

# Changing directories
#setopt auto_pushd					# Make cd push the old directory onto the directory stack.
setopt pushd_ignore_dups		# Dont push copies of the same dir on stack.
setopt pushd_minus				# Reference stack entries with "-".

setopt extended_glob				# Treat the ‘#’, ‘~’ and ‘^’ characters as part of patterns for filename generation, etc. (An initial unquoted ‘~’ always produces named directory expansion.)

# Miscellaneous settings
setopt INTERACTIVE_COMMENTS  # Enable comments in interactive shell.
unsetopt HIST_BEEP

# ░█▀█░█░░░▀█▀░█▀█░█▀▀
# ░█▀█░█░░░░█░░█▀█░▀▀█
# ░▀░▀░▀▀▀░▀▀▀░▀░▀░▀▀▀
csource "$HOME/.aliasrc"

# ░█░█░█▀▀░█░█░█▀▄░▀█▀░█▀█░█▀▄░█▀▀
# ░█▀▄░█▀▀░░█░░█▀▄░░█░░█░█░█░█░▀▀█
# ░▀░▀░▀▀▀░░▀░░▀▀░░▀▀▀░▀░▀░▀▀░░▀▀▀

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


# ░█▀█░█░░░█░█░█▀▀░▀█▀░█▀█░█▀▀
# ░█▀▀░█░░░█░█░█░█░░█░░█░█░▀▀█
# ░▀░░░▀▀▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀

# Check if zinit is installed
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/z-shell/zinit/main/doc/install.sh)"
fi

csource "$HOME/.zinit/bin/zinit.zsh"

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

zinit ice atload"zpcdreplay" atclone'./zplug.zsh'
zinit light g-plane/zsh-yarn-autocompletions

zinit ice depth=1;

zinit light romkatv/powerlevel10k

zinit wait lucid for \
    memark/zsh-dotnet-completion \
    atinit"zicompinit; zicdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
    zdharma-continuum/history-search-multi-word \
    memark/zsh-dotnet-completion \
    urbainvaes/fzf-marks \
    hlissner/zsh-autopair \
    junegunn/fzf-bin \
    atload"zicdreplay" \
        zsh-users/zsh-completions \
        zsh-users/zsh-autosuggestions

# zinit wait lucid for \
#         b4b4r07/enhancd \
#     atinit"zicompinit; zicdreplay" \
#         zsh-users/zsh-syntax-highlighting \
#     atload"_zsh_autosuggest_start; zicdreplay" \
#         zsh-users/zsh-autosuggestions \
#         memark/zsh-dotnet-completion \
#     blockf atpull'zinit creinstall -q .' \
#         zsh-users/zsh-completions \
#         g-plane/zsh-yarn-autocompletions

### End of Zinit's installer chunk

# ░█▀▀░█▀█░█▄█░█▀█░█░░░█▀▀░▀█▀░▀█▀░█▀█░█▀█░█▀▀
# ░█░░░█░█░█░█░█▀▀░█░░░█▀▀░░█░░░█░░█░█░█░█░▀▀█
# ░▀▀▀░▀▀▀░▀░▀░▀░░░▀▀▀░▀▀▀░░▀░░▀▀▀░▀▀▀░▀░▀░▀▀▀

zstyle ':completion:*' rehash true
# zstyle ':completion:*' verbose yes
# zstyle ':completion:*:descriptions' format '%B%d%b'
# zstyle ':completion:*:messages' format '%d'
# zstyle ':completion:*:warnings' format 'No matches for: %d'
# zstyle ':completion:*' group-name ''

# Case-insensitive (all), partial-word and then substring completion
zstyle ":completion:*" matcher-list \
"m:{a-zA-Z}={A-Za-z}" \
"r:|[._-]=* r:|=*" \
"l:|=* r:|=*"

zstyle ":completion:*:default" list-colors ${(s.:.)LS_COLORS}

# Add some completions settings
setopt ALWAYS_TO_END     # Move cursor to the end of a completed word.
setopt AUTO_LIST         # Automatically list choices on ambiguous completion.
setopt AUTO_MENU         # Show completion menu on a successive tab press.
setopt AUTO_PARAM_SLASH  # If completed parameter is a directory, add a trailing slash.
setopt COMPLETE_IN_WORD  # Complete from both ends of a word.
unsetopt MENU_COMPLETE   # Do not autoselect the first completion entry.

zstyle ':completion:*' menu select=2
# zstyle ':completion:*' rehash true                              # automatically find new executables in path
# zstyle ':completion:*' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)*==34=34}:${(s.:.)LS_COLORS}")'  # Colored completion (different colors for dirs/files/etc)

# zstyle ':completion:*' completer _expand _complete _ignored _approximate
# zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
# zstyle ':completion:*:descriptions' format '%U%F{cyan}%d%f%u'

# Load SSH and GPG agents via keychain.

setup_agents() {
    [[ $UID -eq 0 ]] && return
    
    if which keychain &> /dev/null; then
        local -a ssh_keys gpg_keys
        for i in ~/.ssh/**/*pub; do test -f "$i(.N:r)" && ssh_keys+=("$i(.N:r)"); done
        gpg_keys=$(gpg -K --with-colons 2>/dev/null | awk -F : '$1 == "sec" { print $5 }')
        if (( $#ssh_keys > 0 )) || (( $#gpg_keys > 0 )); then
            alias run_agents='() { $(whence -p keychain) --quiet --eval --inherit any-once --agents ssh,gpg $ssh_keys ${(f)gpg_keys} }'
            # [[ -t ${fd:-0} || -p /dev/stdin ]] && eval `run_agents`
            unalias run_agents
        fi
    fi
}

setup_agents
unfunction setup_agents

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

# Source local zsh customizations.
csource ~/.zsh_rclocal
csource ~/.custom/.zshrc

# Source functions and aliases.
csource ~/.zsh_functions

# ░█▀▀░█▀█░█░░░█▀█░█▀▄░▀█▀░▀▀█░█▀▀
# ░█░░░█░█░█░░░█░█░█▀▄░░█░░▄▀░░█▀▀
# ░▀▀▀░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀▀▀

# grc colorizes the output of a lot of commands. If the user installed it, use it.

# Try and find the grc setup file
if (( $+commands[grc] )); then
    GRC_SETUP='/usr/local/etc/grc.bashrc'
fi

if (( $+commands[grc] )) && (( $+commands[brew] ));then
    GRC_SETUP="$(brew --prefix)/etc/grc.bashrc"
fi

if [[ -r "$GRC_SETUP" ]]; then
    source "$GRC_SETUP"
fi

unset GRC_SETUP

if (( $+commands[grc] )); then
    function ping5(){
        grc --color=auto ping -c 5 "$@"
    }
else
    alias ping5='ping -c 5'
fi

# These need to be done after $PATH is set up so we can find
# grc and exa

# When present, use exa instead of ls
if (( $+commands[exa] )); then
    if [[ -z "$EXA_TREE_IGNORE" ]]; then
        EXA_TREE_IGNORE=".cache|cache|node_modules|vendor|.git"
    fi
fi

# ░█▀█░█▀█░█░█░█▀▀░█▀▄░█░░░█▀▀░█░█░█▀▀░█░░░▀█░░▄▀▄░█░█
# ░█▀▀░█░█░█▄█░█▀▀░█▀▄░█░░░█▀▀░▀▄▀░█▀▀░█░░░░█░░█//█░█▀▄
# ░▀░░░▀▀▀░▀░▀░▀▀▀░▀░▀░▀▀▀░▀▀▀░░▀░░▀▀▀░▀▀▀░▀▀▀░░▀░░▀░▀

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

csource "$HOME/.scripts/ranger.zsh"

colorscript -e six
colorscript -e hex
