function pai --wraps=paru\ -Slq\ \|\ fzf\ --multi\ --preview\ \'paru\ -Si\ \{1\}\'\ \|\ xargs\ -ro\ sudo\ paru\ -S --wraps=paru\ -Slq\ \|\ fzf\ --multi\ --preview\ \'paru\ -Si\ \{1\}\'\ \|\ xargs\ -ro\ paru\ -S --description alias\ parusi=paru\ -Slq\ \|\ fzf\ --multi\ --preview\ \'paru\ -Si\ \{1\}\'\ \|\ xargs\ -ro\ paru\ -S
  paru -Slq | fzf --multi --preview 'paru -Si {1}' | xargs -ro paru -S --skipreview --noconfirm --removemake --cleanafter $argv;
end
