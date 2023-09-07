# Node
alias nr='npm run'
alias ni='npm i'
alias nig='npm i --location=globa'
# Docker
alias dk='doas docker'
alias dki='dk images'
# Pacman
alias paii='paru -Qi'
alias pain='paru -S --removemake --cleanafter --skipreview'
alias pais='paru -Ss'
alias paid='pain --asdeps'
alias pai='pain --needed --noconfirm'
alias pau='pai -yyu --upgrademenu'
alias par='paru -Rcnsu'
alias pale='paru -Qqe'
alias pald='paru -Qqd'
# Productivity
alias cp='cp -R'
alias cls='clear && colorscript -r'
alias df='df -h'
alias rm='rm -r'
alias free='free -m'
alias srm='doas rm -r'
alias mkdir='mkdir -p'
alias sd='fd -i -c auto -t d'
alias sf='fd -i -c auto -t f'
alias wifi='doas nmcli d wifi'
alias vpn='doas openvpn'
# Git
alias gts='git status'
alias gtm='git merge'
alias gtg='git log --graph --max-count=5'
alias gtc='git checkout'
alias gtct='git commit -m'
alias gtcm='git checkout master'
alias gtcd='git checkout develop'
alias gtpll='git pull origin $(git branch --show-current)'
alias gtpsh='git push origin $(git branch --show-current)'
# List Directories
alias ls='exa --group-directories-first --icons --all'
alias lsi='exa --group-directories-first --icons --all -l'
alias lsl='exa --group-directories-first --icons --all -T --ignore-glob=".git|node_modules" -L 2'
# Emacs
alias sv='doas emacsclient -nw'
alias v='emacsclient -nw'
alias vim='emacsclient -nw'
alias emacs='emacsclient -c -a "emacs"'
alias doomsync='~/.emacs.d/bin/doom sync'
alias doomdoctor='~/.emacs.d/bin/doom doctor'
alias doomupgrade='~/.emacs.d/bin/doom upgrade'
alias doompurge='~/.emacs.d/bin/doom purge'
# Functions
paclean() {
  paru -Sc --noconfirm
  par --noconfirm $(pald -t)
}

padeps() {
  paid --noconfirm $(paii $@ | sed -n '/^Optional Deps/,/^Req/p' | sed 's/^Req.*//g' | sed 's/^Opt.* : //g' | sed '$d' | sed 's/:.*//g' | sed 's/^\s*//g' | sed 's/^None.*//g' | tr '\n' ' ')
}

nvms() {
  echo $@ >./.nvmrc
}
