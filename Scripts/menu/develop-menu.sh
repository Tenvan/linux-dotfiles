#!/bin/bash

myFileManager="thunar"
myTerminal="alacritty"
myBrowser="firefox"
workDir=$WORK_DIR
shellCmd="$myTerminal -t OneTimeConsole --working-directory $workDir"

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
    "Yarn quick install" "$shellCmd --hold -e yarn install --ignore-scripts" \
    "Yarn full install" "$shellCmd --hold -e yarn" \
    "Start Server" "$shellCmd --hold -e yarn server:dev" \
    "Pug watch" "$shellCmd/src/client --hold -e yarn pug:watch" \
    "Start Default" "$shellCmd/src/client --hold -e yarn start" \
    "Start HMR" "$shellCmd/src/client --hold -e yarn start:client:hmr --port 4201" \
    "Start AOT" "$shellCmd/src/client --hold -e yarn start:client:dev --aot --port 4202" \
    "Generate" "$shellCmd --hold -e yarn generate" \
    "Check Client Updates" "$shellCmd --hold -e yarn outdated" \
    "Check Server Updates" "$shellCmd --hold -e yarn --cwd src/server4 outdated" \
    "Client Upgrade" "$shellCmd --hold -e yarn upgrade" \
    "Server Upgrade" "$shellCmd --hold -e yarn --cwd src/server4 upgrade" \
    "Upgrade Full" "$shellCmd --hold -e yarn run update:all" \
    "Shell" "$shellCmd --hold" \
    "Dateien" "$myFileManager $workDir"
}

choice=$(select_application)

if [ -z "$choice" ]; then
  echo "abort choice"
else
  echo exceute: $choice
  $choice &
fi
