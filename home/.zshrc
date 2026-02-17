# ~/.zshrc

# Cargar entorno común también en shells no-login
if [ -r "$HOME/.config/zsh/env.zsh" ]; then
  source "$HOME/.config/zsh/env.zsh"
elif [ -r "$HOME/dotFiles/home/.config/zsh/env.zsh" ]; then
  source "$HOME/dotFiles/home/.config/zsh/env.zsh"
fi

# Solo sensible para sesiones interactivas
case $- in
  *i*)
    if [ -r "$HOME/.config/zsh/interactive.zsh" ]; then
      source "$HOME/.config/zsh/interactive.zsh"
    elif [ -r "$HOME/dotFiles/home/.config/zsh/interactive.zsh" ]; then
      source "$HOME/dotFiles/home/.config/zsh/interactive.zsh"
    fi
  ;;
esac
