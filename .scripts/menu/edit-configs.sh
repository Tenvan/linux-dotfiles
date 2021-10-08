#!/usr/bin/env bash
echo "# --> $BASH_SOURCE"

# Function create a scale dialog
filesEdit="code -a --file-uri"
folderEdit="code -a --folder-uri"
# filesEdit="atom "
# folderEdit="atom "

ACTIONS=(
    "🇬 Editor" "code"
    "🇬 Workspace" "code -r $HOME/dotfiles.code-workspace"
    "📑 shell configs" "$filesEdit $HOME/.profile* $HOME/.xprofile* $HOME/.xsession* $HOME/.Xresources* $HOME/.zshrc* $HOME/.alias* $HOME/.bashrc*"
    "📁 custom dir" "$folderEdit $HOME/.custom"
    "📁 config dir" "$folderEdit $HOME/.config"
    "📁 awesome dir" "$folderEdit $HOME/.config/awesome"
    "📁 bin dir" "$folderEdit $HOME/.bin"
    "📁 scripts dir" "$folderEdit $SCRIPTS"
    "📁 menu dir" "$folderEdit $SCRIPTS/menu"
    "📑 scripts install" "$filesEdit $SCRIPTS/install*"
    "📑 themes Files" "$filesEdit $HOME/.Xresources* $HOME/.gtkrc-*"
    "📁 themes Folder" "$folderEdit  $HOME/.config/gtk-3.0 $HOME/.config/gtk-4.0 $HOME/.config/qt5ct $HOME/.config/fontconfig"
    "📑 screenlayout" "$filesEdit $HOME/.screenlayout/*"
    "📑 git files" "$filesEdit $HOME/.gitignore* $HOME/.gitconfig*"
)

LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
MLINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($MLINEHEIGHT + $LINEOFFSET))

get_config_list() {
    yad --center --on-top --sticky \
        --list \
        --no-headers \
        --width=400 \
        --height=$HEIGHT \
        --title="Edit Konfiguation" \
        --text="Konfig file" \
        --column="Option" \
        --column="Datei" \
        --separator=" " \
        --print-column=2 \
        --hide-column=2 \
        "${ACTIONS[@]}"
}

choice=$(get_config_list)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo $choice
    $choice &
fi
