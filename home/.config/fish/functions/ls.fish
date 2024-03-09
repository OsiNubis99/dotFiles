function ls --wraps=exa --description 'alias ls=exa --group-directories-first --icons --all'
  exa --group-directories-first --icons --all $argv
end
