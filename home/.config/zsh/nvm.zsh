# NVM para zsh (equivalente a tu ~/.zshrc actual)
export NVM_DIR="$HOME/.nvm"

# Cargar nvm: primero ~/.nvm (instalación directa), luego homebrew como fallback
if [ -s "$NVM_DIR/nvm.sh" ]; then
  source "$NVM_DIR/nvm.sh"
elif command -v brew >/dev/null 2>&1 && [ -s "$(brew --prefix 2>/dev/null)/opt/nvm/nvm.sh" ]; then
  source "$(brew --prefix)/opt/nvm/nvm.sh"
fi

# Completion (opcional)
if [ -n "$_nvm_prefix" ] && [ -s "$_nvm_prefix/opt/nvm/etc/bash_completion.d/nvm" ]; then
  source "$_nvm_prefix/opt/nvm/etc/bash_completion.d/nvm"
elif [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ]; then
  source "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
elif [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ]; then
  source "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
fi

unset _nvm_prefix
