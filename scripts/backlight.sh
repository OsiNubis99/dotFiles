#!/bin/bash

# You can call this script like this:
# backlight.sh up
# backlight.sh down

function get_backlight {
  xbacklight -get | cut -d '.' -f 1
}

function send_notification {
    backlight=`get_backlight`
    dunstify -t 5000 -r 1 -u normal "Backlight" -h "int:value:$backlight"
}

case $1 in
  up)
	  xbacklight -inc 20 > /dev/null
	  send_notification
	  ;;
  down)
	  xbacklight -dec 20 > /dev/null
	  send_notification
	  ;;
esac