export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export EDITOR=$(which nvim)
export TERM=alacritty

# fix "xdg-open fork-bomb" export your preferred browser from here
export BROWSER=$(which firefox-developer-edition)

[ -r $HOME/.profile.custom ] && . $HOME/.profile.custom

# Some Vars settings in '.profile.custom':
# export WORK_DIR=<Work Folder>
# export BW_SESSION=<Bitwarden Session Token>
