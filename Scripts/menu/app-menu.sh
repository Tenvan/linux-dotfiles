#!/bin/bash

myFileManager="thunar"
myTerminal="alacritty"
myBrowser="$BROWSER"

# Function create a scale dialog
select_application() {
    zenity --list \
           --width=400 \
           --height=500 \
           --title="Edit Konfiguation" \
           --text="APPLICATIONS" \
           --column=Option \
           --column="Aktion" \
           --separator=" " \
           --print-column=2 \
           --hide-column=2 \
           --hide-header \
           "🌍 Browser" "$myBrowser" \
           "😃 Emoji Test" "$myTerminal --hold -e curl https://unicode.org/Public/emoji/5.0/emoji-test.txt" \
           "☦ UTF8 Test" "$myTerminal --hold -e curl https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html" \
           "🌤 Wetter Brakel" "$myTerminal --hold -d 140 44 -t Wetter:Brakel -e curl wttr.in/33034?lang=de" \
           "🌤 Wetter Höxter" "$myTerminal --hold -d 140 44 -t Wetter:Höxter -e curl wttr.in/37671?lang=de" \
           "🌤 Wetter Mainz" "$myTerminal --hold -d 140 44 -t Wetter:Mainz -e curl wttr.in/Mainz?lang=de" \
           "♻ Matrix" "$myTerminal --hold -t matrix -e cmatrix" \
           "🚧 xsession Errors" "$myTerminal -t xsession-errors -d 140 44 -e multitail -i $HOME/.xsession-errors" \
           "🚧 Awmtt Start" "$myTerminal --hold -t AWMTT -d 140 44 -e awmtt start  -C .config/awesome/rc.test.lua -S 2000x1500" \
           "🚧 Awmtt Restart" "$myTerminal -t xsession-errors -d 140 44 -e awmtt restart" \
           "🚧 Awmtt Stop" "$myTerminal -t xsession-errors -d 140 44 -e awmtt stop"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exceute: $choice  >>/dev/stderr
    $choice &
fi
