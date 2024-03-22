function par --wraps=paru\ -Qq\ \|\ fzf\ --multi\ --preview\ \'paru\ -Qi\ \{1\}\'\ \|\ xargs\ -ro\ sudo\ paru\ -Rns --description alias\ pacrem=paru\ -Qq\ \|\ fzf\ --multi\ --preview\ \'paru\ -Qi\ \{1\}\'\ \|\ xargs\ -ro\ sudo\ paru\ -Rns
  paru -Qq | fzf --multi --preview 'paru -Qi {1}' | xargs -ro paru -Rncsu $argv;
end
