# Develop
## Node
alias nr="npm run"
alias ni="npm i"

# Productivity
## GNU / Linux
alias cp="cp -i"                                                # Confirm before overwriting something
alias cp="mv -i"                                                # Confirm before overwriting something
alias df="df -h"                                                # Human-readable sizes
alias lsa="ls -A"
alias lsl="lsa -l"
alias free="free -m"                                            # Show sizes in MB
alias edi="sudo nano"
alias srm="sudo rm -r"
alias ls="ls --color=auto"
alias gitu="git add . && git commit && git push"
## Pacman
alias pai="paru -S --cleanafter --removemake --skipreview"
alias pain="paru -S --needed --noconfirm"
alias pais="paru -Ss"
alias pau="paru -Syu --noconfirm"
alias par="paru -Rcnsu"
alias pale="paru -Qqe"
alias pald="paru -Qqd"
alias paclean="par $(pald -t)"

# Functions
cdl ()
{
  if [ -d $1 ] ; then
    cd $1;
    lsa;
  else
    echo "$1 is not a folder";
  fi
}

pasave()
{
for app in "$@"
  do
    echo "Save $app to ~/dotFiles/apps file"
    echo $app >> ~/dotFiles/apps
  done
}

patest()
{
for app in "$@"
  do
    echo "Save $app to ~/dotFiles/apps file"
    echo $app >> ~/dotFiles/apps
  done
}