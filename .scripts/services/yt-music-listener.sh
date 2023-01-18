#!/usr/bin/env bash

# start spotify parser service
playerctl metadata -p chromium --format '{ "artist": "{{xesam:artist}}", "title": "{{xesam:title}}", "album": "{{xesam:album}}", "trackid": "{{mpris:trackid}}" }' --follow
