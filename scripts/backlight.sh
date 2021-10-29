#!/bin/bash

# You can call this script like this:
# backlight.sh up 10
# backlight.sh down 10

function get_backlight {
  light -G | cut -d '.' -f 1
}

function send_notification {
  backlight=`get_backlight`
  dunstify -t 5000 -r 1 -u normal "Backlight" -h "int:value:$backlight"
}

case $1 in
  up)
    light -A $2
    send_notification
    ;;
  down)
    light -U $2
    send_notification
    ;;
esac
