dirs=(ls -d /home/andres/wallpapers/*)
if ((${#dirs[@]} > $1))
	then 
		nitrogen --random --set-zoom-fill "${dirs[$1]}"
	else 
		nitrogen --random --set-zoom-fill /home/andres/wallpapers/
fi
