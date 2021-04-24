i=1
for dir in $(ls -F /home/andres/dotFiles/backup/wallpapers/Pc/ | grep \/$)
do
  notify-send $i $dir -t 5000
	i=$((i+1))
done