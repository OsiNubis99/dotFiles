export TERM="xterm-256color"
export LANG="en_US.UTF-8"
export MY_USER="andres"
export PATH="/home/${MY_USER}/.local/bin":$PATH
export ZDOTDIR="/home/${MY_USER}/dotFiles/config/zsh"
export HISTORY_IGNORE="(clear|cdl|ls|cd|pwd|exit|sudo reboot|history|cd -|cd ..|cdl ..)"
#      Alias
source "${ZDOTDIR}/alias.zsh"
#      Imports 
source "${ZDOTDIR}/zsh-autosuggestions.zsh"
source "${ZDOTDIR}/zsh-history-substring-search.zsh"
#      Plugins  
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#      Settings
source "${ZDOTDIR}/settings.zsh"
#      Theme
source "${ZDOTDIR}/theme.zsh"
#      NVM
source /usr/share/nvm/init-nvm.sh
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"
  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")
    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc

load-nvmrc
colorscript -r
