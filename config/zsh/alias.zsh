# Node
alias nr="npm run"
alias ni="npm i"
# Pacman
alias pain="paru -S --removemake --cleanafter --skipreview"
alias pais="paru -Ss"
alias paid="pain --asdeps"
alias pai="pain --needed --noconfirm"
alias pau="pai -yyu --upgrademenu"
alias par="paru -Rcnsu"
alias pale="paru -Qqe"
alias pald="paru -Qqd"
# Productivity
alias cd="cdl"
alias cp="cp -r"
alias mv="mv -r"
alias df="df -h"
alias rm="rm -r"
alias lsl="ls -l"
alias free="free -m"
alias srm="doas rm -r"
alias mkdir="mkdir -p"
alias reboot="doas reboot"
alias ls="ls --color=auto -A"
alias sd="fd -i -c auto -t d"
alias sf="fd -i -c auto -t f"
alias gitu="git add . && git commit && git push"
# vim and emacs
alias vi="nvim"
alias vim="nvim"
alias v="doas nvim"
alias em="/usr/bin/emacs -nw"
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
