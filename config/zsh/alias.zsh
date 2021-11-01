# Node
alias nr="npm run"
alias ni="npm i"
# Productivity
alias cp="cp -i"                                                
alias mv="mv -i"
alias df="df -h"
alias rm="rm -r"
alias lsl="ls -l"
alias free="free -m"
alias srm="doas rm -r"
alias mkdir="mkdir -p"
alias ls="ls --color=auto -A"
alias gitu="git add . && git commit && git push"
# vim and emacs
alias v="doas nvim"
alias em="/usr/bin/emacs -nw"
alias emacs="emacsclient -c -a 'emacs'"
alias doomsync="~/.emacs.d/bin/doom sync"
alias doomdoctor="~/.emacs.d/bin/doom doctor"
alias doomupgrade="~/.emacs.d/bin/doom upgrade"
alias doompurge="~/.emacs.d/bin/doom purge"

# Pacman
alias pain="paru -S --removemake --cleanafter --skipreview"
alias pais="paru -Ss"
alias paid="pain --asdeps"
alias pai="pain --needed --noconfirm"
alias pau="pai -yyu --upgrademenu"
alias par="paru -Rcnsu"
alias pale="paru -Qqe"
alias pald="paru -Qqd"
# Functions
cdl ()
{
  if [ -d $1 ] ; then
    cd $1;
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
