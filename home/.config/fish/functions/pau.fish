function pau --wraps='sudo pacman -Syu' --description 'alias pacupd=sudo pacman -Syu'
  paru -Syu --skipreview --noconfirm --removemake --cleanafter $argv;
end
