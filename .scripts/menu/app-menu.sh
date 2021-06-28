#!/usr/bin/env bash

myFileManager="thunar"
myTerminal="kitty"
timeCmd="/usr/bin/time -v "
myTestLua=$(eval echo $HOME/.config/awesome/rc.test.lua)

ACTIONS=(
        "😃 Emoji Test" "$myTerminal --hold curl https://unicode.org/Public/emoji/5.0/emoji-test.txt" \
        "☦ UTF8 Test" "$myTerminal --hold curl https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html" \
        "🌤 Wetter Brakel" "$myTerminal --hold --title Wetter:Brakel curl wttr.in/33034?lang=de" \
        "🌤 Wetter Höxter" "$myTerminal --hold --title Wetter:Höxter curl wttr.in/37671?lang=de" \
        "🌤 Wetter Mainz" "$myTerminal --hold --title Wetter:Mainz curl wttr.in/Mainz?lang=de"
)

LINECOUNT=$(expr ${#ACTIONS[*]} / 2)
LINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
HEIGHT=$(($LINEHEIGHT + $LINEOFFSET))

# Function create a scale dialog
select_application() {
    zenity --list \
        --width=400 \
        --height=$HEIGHT \
        --title="Edit Konfiguation" \
        --text="APPLICATIONS" \
        --column=Option \
        --column="Aktion" \
        --separator=" " \
        --print-column=2 \
        --hide-column=2 \
        --hide-header \
        "${ACTIONS[@]}"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo excecute: $choice >>/dev/stderr
    $choice &
fi
