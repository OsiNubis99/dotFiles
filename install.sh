#!/usr/bin/env bash

# TODO: Install required packages with homebrew
# brew install -y git stow

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

# Final instalation
echo "Instalation Complete!"
