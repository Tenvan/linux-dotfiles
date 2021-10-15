#!/usr/bin/env bash
. $SCRIPTS/defs

SYSTEM_ACTIONS=(
    "🇯 JetBrains Toolbox" "jetbrains-toolbox"
    "🇹 Teams" "teams"
    "🇫 Font Manager" "font-manager"
    "🇻 Virtio Manager" "virt-manager"
    "🇷 Remmina" "remmina"
    "🗂 Dateimanager" "$FILEMANAGER"
    "😃 Emoji Test" "$TERMINAL --hold curl https://unicode.org/Public/emoji/5.0/emoji-test.txt"
    "☦ UTF8 Test" "$TERMINAL --hold curl https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html"
    "🌤 Wetter Brakel" "$TERMINAL --hold --title Wetter:Brakel curl wttr.in/33034?lang=de"
    "🌤 Wetter Höxter" "$TERMINAL --hold --title Wetter:Höxter curl wttr.in/37671?lang=de"
)

csource "$CUSTOMS/${0##*/}"

ACTIONS=("${CUSTOM_TOP_ACTIONS[@]}" "${SYSTEM_ACTIONS[@]}" "${CUSTOM_BOTTOM_ACTIONS[@]}")
LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
MLINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($MLINEHEIGHT + $LINEOFFSET))

# Function create a scale dialog
select_application() {
    yad --center --on-top --sticky \
    --list \
    --no-headers \
    --width=400 \
    --height=$HEIGHT \
    --title="Edit Konfiguation" \
    --text="Anwendungen" \
    --column="Option" \
    --column="Aktion" \
    --separator=" " \
    --print-column=2 \
    --hide-column=2 \
    "${ACTIONS[@]}"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo excecute: $choice >>/dev/stderr
    $choice &
fi
