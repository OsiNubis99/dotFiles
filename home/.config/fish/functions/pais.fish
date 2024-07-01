function pais --wraps=dnk --description 'alias pais=doas dnf search'
    doas dnf search $argv
end
