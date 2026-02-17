# NVM para zsh (equivalente a tu ~/.zshrc actual)
export NVM_DIR="$HOME/.nvm"

# Homebrew prefix (si existe) + fallback
_nvm_prefix=""
if command -v brew >/dev/null 2>&1; then
  _nvm_prefix="$(brew --prefix 2>/dev/null)"
fi

# Cargar nvm
if [ -n "$_nvm_prefix" ] && [ -s "$_nvm_prefix/opt/nvm/nvm.sh" ]; then
  source "$_nvm_prefix/opt/nvm/nvm.sh"
elif [ -s "/opt/homebrew/opt/nvm/nvm.sh" ]; then
  source "/opt/homebrew/opt/nvm/nvm.sh"
elif [ -s "/usr/local/opt/nvm/nvm.sh" ]; then
  source "/usr/local/opt/nvm/nvm.sh"
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
