#!/bin/bash

myFileManager="thunar"
myTerminal="alacritty"
myBrowser="$BROWSER"

# Function create a scale dialog
select_application() {
  zenity --list \
    --width=300 \
    --height=600 \
    --title="Edit Konfiguation" \
    --text="System Menü" \
    --column=Option \
    --column="Aktion" \
    --separator=" " \
    --print-column=2 \
    --hide-column=2 \
    --hide-header \
    " JetBrains Toolsbox" "jetbrains-toolbox" \
    " SmartGit" "/opt/smartgit/bin/smartgit.sh" \
    " Krusader" "krusader" \
    " Dateien" $myFileManager \
    " xmonad Errors" "$myTerminal -t xmonad-errors -d 140 44 -e multitail -i $HOME/.xmonad/xmonad.errors" \
    " xsession Errors" "$myTerminal -t xsession-errors -d 140 44 -e multitail -i $HOME/.xsession-errors" \
    "  Browser" "$myBrowser" \
    " Teams" "teams" \
    " Emoji Test" "$myTerminal --hold -e curl https://unicode.org/Public/emoji/5.0/emoji-test.txt" \
    " UTF8 Test" "$myTerminal --hold -e curl https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html" \
    "  Wetter Brakel" "$myTerminal --hold -d 140 44 -t wetter -e curl wttr.in/33034?lang=de" \
    "  Wetter Höxter" "$myTerminal --hold -d 140 44 -t wetter -e curl wttr.in/37671?lang=de" \
    " Matrix" "$myTerminal --hold -t matrix -e cmatrix"
}

choice=$(select_application)

if [ -z "$choice" ]; then
  echo "abort choice"
else
    echo exceute: $choice  >>/dev/stderr
    $choice &
fi
