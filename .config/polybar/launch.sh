#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

xrdb ~/.Xresources 

# Launch bar1 and bar2
mkdir -p /tmp/polybar
echo "---" | tee -a /tmp/polybar/main-bottom.log /tmp/polybar/main-top.log /tmp/polybar/second-bottom.log /tmp/polybar/second-top.log

# First/Primary Screen
polybar bar -c ~/.config/polybar/config-main-bottom >> /tmp/polybar/main-bottom.log 2>&1 &
polybar bar -c ~/.config/polybar/config-main-top >> /tmp/polybar/main-top.log 2>&1 &

# Second Screen
polybar bar -c ~/.config/polybar/config-second-bottom >> /tmp/polybar/main-bottom.log 2>&1 &
polybar bar -c ~/.config/polybar/config-second-top >> /tmp/polybar/main-top.log 2>&1 &


notify-send "PolyBars launched..."
