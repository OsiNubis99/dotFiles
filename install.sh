#!/usr/bin/env bash

sudo dnf install -y git stow

# Welcome
echo "Welcome to the Instalation Script"

if [[ ! -d "$HOME/dotFiles" ]]; then
  echo "$HOME/dotFiles folder is required, cloning it"
  cd "$HOME" || exit
  git clone https://github.com/OsiNubis99/dotFiles.git ~/dotFiles
fi

echo "Creating base Folders"
mkdir -p ~/.config
mkdir -p ~/.local/share

echo "Copy /etc configs"
sudo cp -r ~/dotFiles/etc /

## Backup
if [[ -d "$HOME/backup" ]]; then
  echo "Stow backup folder"
  stow -d "$HOME/backup/" -t "$HOME/" .
  if [[ -e "$HOME/.ssh/personal_key" ]]; then
    chmod -R 700 ~/.ssh
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/personal_key
    echo "Personal ssh key added"
  fi
fi

echo "Stow home folder"
stow -d "$HOME/dotFiles/home/" -t "$HOME/" .

## ZSH
rm -r -f ~/.zshrc
ln -s ~/dotFiles/config/zsh/.zshrc ~/.zshrc
rm -r -f ~/.zhistory
ln -s ~/dotFiles/config/zsh/.zhistory ~/.zhistory

# Final instalation
echo "Inastalling all nedeed software..."
cat ~/dotFiles/apps/* | sudo dnf install -y - 1>/dev/null || exit 1
echo "Instalation Complete!"
