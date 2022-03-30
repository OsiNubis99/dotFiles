#!/bin/bash

# You can call this script like this:
# volume.sh up 5
# volume.sh down 5
# volume.sh mute

function get_volume {
  pamixer --get-volume
}

function is_mute {
  pamixer --get-mute
}

function send_notification {
  volume=`get_volume`
  dunstify -t 5000 -r 1 -u normal "Volume" -h "int:value:$volume"
}

case $1 in
  up)
    pamixer -i $2
    send_notification
    ;;
  down)
    pamixer -d $2
    send_notification
    ;;
  mute)
    pamixer -t
    if is_mute ; then
      dunstify -t 5000 -r 1 -u normal "Mute"
    else
      send_notification
    fi
    ;;
esac
