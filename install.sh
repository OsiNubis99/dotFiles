#!/usr/bin/env bash

sudo pacman -Syu --needed --noconfirm git stow base linux linux-headers linux-firmware emacs

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
mkdir -p ~/Documents
mkdir -p ~/Downloads
mkdir -p ~/Music
mkdir -p ~/Pictures
mkdir -p ~/Public
mkdir -p ~/Videos

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

## Doom
if [[ ! $(~/.emacs.d/bin/doom info 2>/dev/null) ]]; then
  git clone --depth 1 --single-branch https://github.com/doomemacs/doomemacs ~/.config/emacs
  ~/.config/emacs/bin/doom install --no-config
fi

## Install paru only if it doesn't already exist
if [[ ! $(which paru 2>/dev/null) ]]; then
  sudo rm -r /home/andres/.cache/paru
  git clone https://aur.archlinux.org/paru.git ~/.cache/paru
  cd ~/.cache/paru/ || exit 1
  makepkg -si
  cd ~/ || exit 1
  sudo rm -r /home/andres/.cache/paru
fi

# Final instalation
echo "Inastalling all nedeed software..."
cat ~/dotFiles/apps/* | paru -Syu --asexplicit --noconfirm - 1>/dev/null || exit 1
echo "Instalation Complete!"
