function nvm_hook --on-variable PWD
  set --local node_version "$(nvm version)"
  set --local nvmrc_path "$(nvm_find_nvmrc)"
  echo $path_
  if test -e "$nvmrc_path"
    set --local nvmrc_node_version $(nvm version "$(cat "$nvmrc_path")")
    if test "$nvmrc_node_version" = "N/A"
      nvm install
    else if test "$nvmrc_node_version" != "$node_version"
      nvm use
    end
  else if test "$node_version" != "$(nvm version default)"
    nvm use default
  end
end

function nvm_find_nvmrc
  set path_ "$PWD"
  while test $path_ != "/" ; and ! test -e "$path_/.nvmrc"
    set path_ $(dirname $path_)
  end
  printf "$path_/.nvmrc"
end
