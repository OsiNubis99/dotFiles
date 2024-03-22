function rm --wraps=rm --description 'alias rm=rm -R'
  /bin/rm -R $argv;
end
