function par --wraps=dnk --description 'alias par=doas dnf remove'
    doas dnf remove $argv
end
