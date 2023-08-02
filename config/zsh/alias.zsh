# Node
alias nr="npm run"
alias ni="npm i"
alias nig="npm i --location=globa"
alias nvmg="echo \"stable\" > ./.nvmrc"
alias nvmu="cd ."
# Docker
alias dk="doas docker"
alias dki="dk images"
# Pacman
alias paii="paru -Qi"
alias pain="paru -S --removemake --cleanafter --skipreview"
alias pais="paru -Ss"
alias paid="pain --asdeps"
alias pai="pain --needed --noconfirm"
alias pau="pai -yyu --upgrademenu"
alias par="paru -Rcnsu"
alias pale="paru -Qqe"
alias pald="paru -Qqd"
# Productivity
alias cp="cp -R"
alias cls="clear && colorscript -r"
alias df="df -h"
alias rm="rm -r"
alias free="free -m"
alias srm="doas rm -r"
alias mkdir="mkdir -p"
alias sd="fd -i -c auto -t d"
alias sf="fd -i -c auto -t f"
alias wifi="doas nmcli d wifi"
alias vpn="doas openvpn"
# List Directories
alias ls='exa --group-directories-first --icons --all'
alias lsi='exa --group-directories-first --icons --all -l'
alias lsl='exa --group-directories-first --icons --all -T --ignore-glob=".git|node_modules" -L 2'
# vim and emacs
alias sv="doas nvim"
alias v="nvim"
alias vim="nvim"
alias emacs="emacsclient -c -a 'emacs'"
alias doomsync="~/.emacs.d/bin/doom sync"
alias doomdoctor="~/.emacs.d/bin/doom doctor"
alias doomupgrade="~/.emacs.d/bin/doom upgrade"
alias doompurge="~/.emacs.d/bin/doom purge"
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
