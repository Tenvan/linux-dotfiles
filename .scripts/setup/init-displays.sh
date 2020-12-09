#!/usr/bin/env bash
# set -n
# set -x
# set -v
# set -e

source $HOME/Scripts/defs.sh

get_config_list() {
  mmonitors=("${@}")
  delimiter="connected"

  echo Anzahl Screens: "${#mmonitors[@]}"

  for ((c = 0; c < ${#mmonitors[@]}; c++)); do
    line=${mmonitors[$c]}
    port=$(cut -f 1 -d ' ' <<<$line)
    desc=$(cut -f 3- -d ' ' <<<$line)

    echo Port: $port
    echo Info: $desc

    # awk '{print $1}' <<< $mmonitors[$c]
    # awk '{print $2}' <<< $mmonitors[$c]
    declare -a screen

    screen+=("$port")
    screen+=("$desc")

  done

  echo Screen count.: ${#screen[@]}
  echo Screen.......: ${screen[@]}

  for index in ${!screen[*]}; do
    printf "%4d: %s\n" $index "${screen[$index]}"
    params=$"$params \"${screen[$index]}\""
  done

  echo "Params: >>$params<<"

  command="zenity --list --width=400 --height=400 --title='Display' --text='Screen' --hide-header --column='Option' $params"

  echo "Command: >>$command<<"

  iex $command

    
}

monitors=$(xrandr | grep connected -w)

IFS=$'\n' read -r -d '' -a array <<<$monitors

get_config_list "${array[@]}"
