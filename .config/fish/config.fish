#
# ~/.bashrc
#
echo "run: config.fish"

export SCRIPTS="$HOME/.scripts"

test -f "$SCRIPTS/defs.sh" && sh "$SCRIPTS/defs.sh"

test -f "$HOME/.aliasrc" && sh "$HOME/.aliasrc"

### COLOR SCRIPTS ###
test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 3
test -f /opt/shell-color-scripts/colorscript.sh && /opt/shell-color-scripts/colorscript.sh -e 23

### BASH POWERLINE ###
starship init fish | source

test -f /usr/bin/neofetch && neofetch

test -f ~/.config/fish/custom.fish && source ~/.config/fish/custom.fish
