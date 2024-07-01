function pai --wraps=dnk --description 'alias pai=doas dnf install -y'
    doas dnf install -y $argv
end
