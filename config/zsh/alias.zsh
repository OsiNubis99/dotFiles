# Node
alias nr="npm run"
alias ni="npm i"
# Productivity
alias cp="cp -i"                                                
alias cp="mv -i"                                                
alias df="df -h"
alias lsl="lsa -l"
alias free="free -m"                                            
alias edi="sudo nvim"
alias srm="sudo rm -r"
alias ls="ls --color=auto -A"
alias gitu="git add . && git commit && git push"
# Pacman
alias pai="paru -S --removemake --cleanafter --skipreview"
alias pain="pai --needed --noconfirm"
alias pais="paru -Ss"
alias pau="pain -yu"
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