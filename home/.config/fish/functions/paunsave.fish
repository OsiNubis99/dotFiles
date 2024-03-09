function paunsave
  comm -23 (pacman -Qqett | sort | psub) (cat ~/dotFiles/apps/* | xargs -n1 -d'\n' pactree -u 2> /dev/null | sort -u | psub)
end
