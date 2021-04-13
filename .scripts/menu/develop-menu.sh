#!/usr/bin/env bash

myFileManager="thunar"
myTerminal="kitty"
workDir=$WORK_DIR
shellCmd="$myTerminal --directory $workDir"
timeCmd="/usr/bin/time -v "

filesEdit="code -r --file-uri"
folderEdit="code -r --folder-uri"

startServer() {
    $shellCmd --hold --title OTC:StartServer yarn workspace onetime-server run start &
}

startPugWatch() {
    $shellCmd --hold --title OTC:PugWatch yarn workspace onetime-client pug_once &
}

startCompiler() {
    $shellCmd --hold --title OTC:StartDefault yarn workspace onetime-client  start &
}

startAll() {
    killall node
    fuser -k 4200/tcp
    fuser -k 4201/tcp
    fuser -k 4202/tcp
    startServer
    startPugWatch
    startCompiler
}


LINECOUNT=23
LINEHEIGHT=$(($LINECOUNT * $LINEHEIGHT))
OFFSET=120
HEIGHT=$(($LINEHEIGHT + $OFFSET))

ACTIONS=(
    "🇬 Git" "gitahead $WORK_DIR"
    "💽 Yarn quick install" "$shellCmd --hold --title OTC:QuickInstall $timeCmd yarn install"
    "💽 Yarn full install" "$shellCmd --hold --title OTC:FullInstall $timeCmd yarn"
    "🏄 Start Server" "startServer"
    "🇵 Pug watch" "startPugWatch"
    "🛫 Start" "startCompiler"
    "🏄 Start All" "startAll"
    "🇵 Pug once" "$shellCmd --hold --title OTC:PugOnce yarn workspace onetime-client pug_once"
    "⚗ Generate" "$shellCmd --hold --title OTC:Generate $timeCmd yarn generate"
    "🇺 Check Client Updates" "$shellCmd --hold --title OTC:CheckClientUpdates $timeCmd yarn outdated"
    "🇺 Check Server Updates" "$shellCmd --hold --title OTC:CheckServerUpdates $timeCmd yarn --cwd src/server4 outdated"
    "🆙 Client Upgrade" "$shellCmd --hold --title OTC:ClientUpgrade $timeCmd yarn upgrade"
    "🆙 Server Upgrade" "$shellCmd --hold --title OTC:ServerUpgrade $timeCmd yarn --cwd src/server4 upgrade"
    "🥋 Upgrade Full" "$shellCmd --hold --title OTC:FullUpgrade $timeCmd yarn run update:all"
    "💉 Doctor" "$shellCmd --hold --title OTC:Doctor $timeCmd yarn doctor"
    "☑ Doctor Check" "$shellCmd --hold --title OTC:DoctorCheck $timeCmd yarn doctor:check"
    "☑ Client Check" "$shellCmd --hold --title OTC:ClientCheck $timeCmd yarn client:check"
    "✅ Prod Check" "$shellCmd --hold --title OTC:ClientCheck $timeCmd yarn client:check:prod"
    "💻 Shell" "$shellCmd --hold --title OTC:Shell"
    "🛻 SQL-Server Stop" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl stop mssql-server"
    "🛻 SQL-Server Start" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl start mssql-server"
    "🛻 SQL-Server Restart" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl restart mssql-server"
    "📑 Dateien" "$myFileManager $workDir"
)

# Function create a scale dialog
select_application() {
    zenity --list \
        --width=400 \
        --height=$HEIGHT \
        --title="Edit Konfiguation" \
        --text="DEVELOP" \
        --column="Option" \
        --column="Aktion" \
        --print-column=2 \
        --hide-column=2 \
        --hide-header \
        "${ACTIONS[@]}"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exec: $choice >>/dev/stderr
    eval $choice &
fi
