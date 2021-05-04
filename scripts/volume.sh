#!/bin/bash

# You can call this script like this:
# volume.sh up
# volume.sh down
# volume.sh mute

function get_volume {
    amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 2 | cut -d '%' -f 1
}

function is_mute {
    amixer get Master | grep '%' | grep -oE '[^ ]+$' | grep off > /dev/null
}

function send_notification {
    volume=`get_volume`
    dunstify -t 5000 -r 1 -u normal "Volume" -h "int:value:$volume"
}

case $1 in
  up)
	  amixer set Master 5%+ unmute > /dev/null
	  send_notification
	  ;;
  down)
	  amixer set Master 5%- unmute > /dev/null
	  send_notification
	  ;;
  mute)
	  amixer set Master toggle > /dev/null
	  if is_mute ; then
	      dunstify -t 5000 -r 1 -u normal "Mute"
	  else
	      send_notification
	  fi
	  ;;
esac