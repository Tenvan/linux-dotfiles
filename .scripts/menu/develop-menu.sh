#!/usr/bin/env bash

myFileManager="thunar"
myTerminal="kitty"
workDir=$WORK_DIR
shellCmd="$myTerminal --directory $workDir"
timeCmd="/usr/bin/time -v "

filesEdit="code -r --file-uri"
folderEdit="code -r --folder-uri"

startAll() {
    killall node
    fuser -k 4200/tcp
    fuser -k 4201/tcp
    fuser -k 4202/tcp
    $shellCmd --hold --title OTC:StartServer yarn server:dev &
    $shellCmd --hold --title OTC:PugWatch yarn --cwd src/client pug:watch &
    $shellCmd --hold --title OTC:StartDefault yarn --cwd src/client start &
}

# Function create a scale dialog
select_application() {
    zenity --list \
        --width=400 \
        --height=950 \
        --title="Edit Konfiguation" \
        --text="DEVELOP" \
        --column="Option" \
        --column="Aktion" \
        --print-column=2 \
        --hide-column=2 \
        --hide-header \
        "🇬 Git" "gitahead $WORK_DIR" \
        "💽 Yarn quick install" "$shellCmd --hold --title OTC:QuickInstall $timeCmd yarn install --ignore-scripts" \
        "💽 Yarn full install" "$shellCmd --hold --title OTC:FullInstall $timeCmd yarn" \
        "🏄 Start All" "startAll" \
        "🏄 Start Server" "$shellCmd --hold --title OTC:StartServer yarn server:dev" \
        "🇵 Pug watch" "$shellCmd --hold --title OTC:PugWatch yarn --cwd src/client pug:watch" \
        "🇵 Pug once" "$shellCmd --hold --title OTC:PugOnce yarn --cwd src/client pug:once" \
        "🛫 Start" "fuser -k 4200/tcp & $shellCmd --hold --title OTC:StartDefault $timeCmd yarn --cwd src/client start" \
        "⚗ Generate" "$shellCmd --hold --title OTC:Generate $timeCmd yarn generate" \
        "🇺 Check Client Updates" "$shellCmd --hold --title OTC:CheckClientUpdates $timeCmd yarn outdated" \
        "🇺 Check Server Updates" "$shellCmd --hold --title OTC:CheckServerUpdates $timeCmd yarn --cwd src/server4 outdated" \
        "🆙 Client Upgrade" "$shellCmd --hold --title OTC:ClientUpgrade $timeCmd yarn upgrade" \
        "🆙 Server Upgrade" "$shellCmd --hold --title OTC:ServerUpgrade $timeCmd yarn --cwd src/server4 upgrade" \
        "🥋 Upgrade Full" "$shellCmd --hold --title OTC:FullUpgrade $timeCmd yarn run update:all" \
        "💉 Doctor" "$shellCmd --hold --title OTC:Doctor $timeCmd yarn doctor" \
        "☑ Doctor Check" "$shellCmd --hold --title OTC:DoctorCheck $timeCmd yarn doctor:check" \
        "☑ Client Check" "$shellCmd --hold --title OTC:ClientCheck $timeCmd yarn client:check" \
        "✅ Prod Check" "$shellCmd --hold --title OTC:ClientCheck $timeCmd yarn client:check:prod" \
        "💻 Shell" "$shellCmd --hold --title OTC:Shell" \
        "🛻 SQL-Server Stop" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl stop mssql-server" \
        "🛻 SQL-Server Start" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl start mssql-server" \
        "🛻 SQL-Server Restart" "$shellCmd --hold --title OTC:SqlServer $timeCmd sudo systemctl restart mssql-server" \
        "📑 Dateien" "$myFileManager $workDir"
}

choice=$(select_application)

if [ -z "$choice" ]; then
    echo "abort choice"
else
    echo exec: $choice >>/dev/stderr
    eval $choice &
fi
