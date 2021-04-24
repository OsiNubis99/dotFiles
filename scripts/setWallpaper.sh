dirs=($(ls -d /home/andres/dotFiles/backup/wallpapers/Pc/*/))
if ((${#dirs[@]} >= $1 && $1 > 0))
	then 
		nitrogen --random --set-zoom-fill ${dirs[$1]}
	else 
		nitrogen --random --set-zoom-fill /home/andres/dotFiles/backup/wallpapers/Pc/
fi