#!/usr/bin/env bash
echo "# --> $BASH_SOURCE"

class_name=$1

# regex for extracting hex id's
grep_id='0[xX][a-zA-Z0-9]\{7\}'

xprop -spy -root _NET_ACTIVE_WINDOW | grep --line-buffered -o $grep_id |
while read -r id; do
    class="`xprop -id $id WM_CLASS | grep $class_name`"
    echo "event id: $id"
    if [ -n "$class" ]; then
        # Found a window with the correct WM_CLASS now what makes your
        # window unique?
        echo "Get Id: $id / $class_name"        
    fi
done
