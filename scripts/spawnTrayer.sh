pid=$(pidof trayer || echo "0");
if (($pid > 0))
	then
		kill $pid
	else
		trayer --height 24 --distance 3 --tint 0x152429 --padding 3 --iconspacing 15 --edge top --align center --width 25 --alpha 128 --transparent true --expand false &
fi
