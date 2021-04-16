export MY_USER="andres"
export ZDOTDIR="/home/${MY_USER}/dotFiles/config/zsh"
# export HISTORY_IGNORE="(clear|cdl|ls|cd|pwd|exit|sudo reboot|history|cd -|cd ..)"
export QT_QPA_PLATFORMTHEME="qt5ct"

if [[ -e "${ZDOTDIR}/theme.zshrc" ]]; then
  source "${ZDOTDIR}/theme.zshrc"
  else
  echo "Theme file not found"
fi

if [[ -e "${ZDOTDIR}/settings.zshrc" ]]; then
  source "${ZDOTDIR}/settings.zshrc"
  else
  echo "Settings file not found"
fi

if [[ -e "${ZDOTDIR}/alias.zshrc" ]]; then
  source "${ZDOTDIR}/alias.zshrc"
  else
  echo "Alias file not found"
fi
