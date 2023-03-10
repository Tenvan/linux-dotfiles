#!/usr/bin/env bash

# start spotify parser service
playerctl metadata -p spotify --format '{ "artist": "{{xesam:artist}}", "title": "{{xesam:title}}", "status": "{{status}}", "discNumber": {{xesam:discNumber}}, "trackNumber": {{xesam:trackNumber}}, "album": "{{xesam:album}}", "url": "{{xesam:url}}", "trackid": "{{mpris:trackid}}", "artUrl": "{{mpris:artUrl}}" }' --follow
