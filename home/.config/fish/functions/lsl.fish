function lsl --wraps=exa --description 'alias lsl=exa --group-directories-first --icons --all -T --ignore-glob=".git|node_modules" -L 2'
  exa --group-directories-first --icons --all -T --ignore-glob=".git|node_modules" -L 2 $argv
end
