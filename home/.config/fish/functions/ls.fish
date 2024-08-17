function ls --wraps=lsd --description 'alias ls=lsd --group-directories-first --icons --all'
  lsd --group-directories-first --all $argv
end
