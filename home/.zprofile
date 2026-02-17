eval "$(/opt/homebrew/bin/brew shellenv)"

# Cargar entorno común de zsh (equivalente a lo que haces en fish/config.fish)
if [ -r "$HOME/.config/zsh/env.zsh" ]; then
  source "$HOME/.config/zsh/env.zsh"
elif [ -r "$HOME/dotFiles/home/.config/zsh/env.zsh" ]; then
  source "$HOME/dotFiles/home/.config/zsh/env.zsh"
fi
