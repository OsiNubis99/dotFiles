# Node
alias nr="npm run"
alias ni="npm i"
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
alias df="df -h"
alias rm="rm -r"
alias lsl="ls -l"
alias free="free -m"
alias srm="doas rm -r"
alias mkdir="mkdir -p"
alias ls="ls --color=auto -A"
alias sd="fd -i -c auto -t d"
alias sf="fd -i -c auto -t f"
alias wifi="doas nmcli d wifi"
alias vpn="doas openvpn"
# vim and emacs
alias vi="nvim"
alias vim="nvim"
alias v="doas nvim"
alias emacs="emacsclient -c -a 'emacs'"
alias doomsync="~/.emacs.d/bin/doom sync"
alias doomdoctor="~/.emacs.d/bin/doom doctor"
alias doomupgrade="~/.emacs.d/bin/doom upgrade"
alias doompurge="~/.emacs.d/bin/doom purge"
# Functions
cdl ()
{
  if [ -d $1 ] ; then
    cd $@;
    ls;
  else
    echo "$1 is not a folder";
  fi
}
paclean()
{
  paru -Sc --noconfirm;
  par --noconfirm $(pald -t);
}
padeps()
{
  paid --noconfirm $(paii $@ | sed -n '/^Optional Deps/,/^Req/p' | sed 's/^Req.*//g'  | sed 's/^Opt.* : //g' | sed '$d' | sed 's/:.*//g' | sed 's/^\s*//g' | sed 's/^None.*//g' | tr '\n' ' ');
}
