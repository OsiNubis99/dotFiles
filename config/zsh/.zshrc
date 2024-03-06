export MY_USER="andres"
export ZDOTDIR="/home/${USER}/dotFiles/config/zsh"
#      My Env
source "${ZDOTDIR}/env.zsh"
#      Alias
source "${ZDOTDIR}/alias.zsh"
#      Settings
source "${ZDOTDIR}/settings.zsh"
#      Theme
source "${ZDOTDIR}/theme.zsh"
#      Plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source /usr/share/zsh/plugins/zsh-hist/zsh-hist.plugin.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
#      SSH
eval "$(ssh-agent)" 1> /dev/null
#      NVM
source /usr/share/nvm/init-nvm.sh
#      FUCK
eval $(thefuck -r -a fuck)
#      ZOXIDE
eval "$(zoxide init --cmd cd zsh)"
# export _ZO_RESOLVE_SYMLINKS = 1

autoload -U add-zsh-hook
loadNVMRC() {
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
add-zsh-hook chpwd loadNVMRC
loadNVMRC
