# Configuración interactiva para zsh (solo cuando hay TTY)

# Modo vi como en fish_vi_key_bindings
bindkey -v

# zoxide (mismo comando 'cd' que fish)
if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init --cmd cd zsh)"
fi

# starship prompt (usa tu ~/.config/starship.toml)
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init zsh)"
fi

# fzf: intenta cargar keybindings/completion instalados por brew
# (equivalente funcional a tu conf.d/fzf.fish)
if command -v fzf >/dev/null 2>&1; then
  if [ -r "$(brew --prefix 2>/dev/null)/opt/fzf/shell/key-bindings.zsh" ]; then
    source "$(brew --prefix)/opt/fzf/shell/key-bindings.zsh"
  elif [ -r "/opt/homebrew/opt/fzf/shell/key-bindings.zsh" ]; then
    source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"
  fi

  if [ -r "$(brew --prefix 2>/dev/null)/opt/fzf/shell/completion.zsh" ]; then
    source "$(brew --prefix)/opt/fzf/shell/completion.zsh"
  elif [ -r "/opt/homebrew/opt/fzf/shell/completion.zsh" ]; then
    source "/opt/homebrew/opt/fzf/shell/completion.zsh"
  fi

  # Variables similares a las universales de fish
  export FZF_TMUX_HEIGHT=${FZF_TMUX_HEIGHT:-"40%"}
  export FZF_DEFAULT_OPTS=${FZF_DEFAULT_OPTS:-"--height $FZF_TMUX_HEIGHT"}
fi

# Aliases equivalentes a tus functions de fish
if [ -r "$HOME/.config/zsh/aliases.zsh" ]; then
  source "$HOME/.config/zsh/aliases.zsh"
elif [ -r "$HOME/dotFiles/home/.config/zsh/aliases.zsh" ]; then
  source "$HOME/dotFiles/home/.config/zsh/aliases.zsh"
fi

# NVM (mantiene lo que ya tenías en ~/.zshrc)
if [ -r "$HOME/.config/zsh/nvm.zsh" ]; then
  source "$HOME/.config/zsh/nvm.zsh"
elif [ -r "$HOME/dotFiles/home/.config/zsh/nvm.zsh" ]; then
  source "$HOME/dotFiles/home/.config/zsh/nvm.zsh"
fi

# Completions
autoload -Uz compinit
compinit -u
