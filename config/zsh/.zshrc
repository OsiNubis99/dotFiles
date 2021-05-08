export TERM="xterm-256color"
export LANG="en_US.UTF-8"
export MY_USER="andres"
export PATH="/home/${MY_USER}/.local/bin":$PATH
export ZDOTDIR="/home/${MY_USER}/dotFiles/config/zsh"
# export HISTORY_IGNORE="(clear|cdl|ls|cd|pwd|exit|sudo reboot|history|cd -|cd ..)"
#      Alias
source "${ZDOTDIR}/alias.zsh"
#      Imports  
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
#      Settings
source "${ZDOTDIR}/settings.zsh"
#      Theme
source "${ZDOTDIR}/theme.zsh"