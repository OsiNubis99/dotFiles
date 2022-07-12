#!/usr/bin/env bash

## Clean temp file
echo "" > ~/dotFiles/app.temp

## Read flags
ASK=false
while getopts 'yh' OPTION; do
	case "$OPTION" in
		h)
			echo "Fast install script"
			printf "\n\t-y Avoid confirm every step"
			printf "\n\t-h Print this message\n"
			exit 0
			;;
		y)
			ASK=true
			REPLY="Y"
			;;
		?)
			echo "Use with -y to avoid confirm every step"
			exit 1
			;;
	esac
done

# Welcome
echo "Welcome to the Instalation Script"
echo "Create folders and copy globals settings"

## Creating Folders
mkdir -p ~/.config
mkdir -p ~/.local/share
mkdir -p ~/Documents
mkdir -p ~/Downloads
mkdir -p ~/Music
mkdir -p ~/Pictures
mkdir -p ~/Public
mkdir -p ~/Videos

## ETC configs
sudo cp -r ~/dotFiles/config/etc /

## Fontconfig
rm -r -f ~/.local/share/fonts
ln -s ~/dotFiles/config/fonts ~/.local/share/fonts
rm -r -f ~/.config/fontconfig
ln -s ~/dotFiles/config/fontconfig ~/.config/fontconfig

## Git
if [[ ! $(which git 2> /dev/null) ]]
then
  sudo pacman -Syu --needed --noconfirm git
fi
rm -r -f ~/.gitconfig
ln -s ~/dotFiles/config/git/gitconfig ~/.gitconfig

## SSH
$ASK || ( echo -n "Do you have your backup folder? (Wakatime and Wallpapers) [Y/n]" && read )
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	rm -r -f ~/.ssh
	mkdir ~/.ssh
	cp ~/dotFiles/backup/ssh/* ~/.ssh/
	if [[ -e "$HOME/.ssh/id_ed25519" ]]; then
		chmod -R 700 ~/.ssh
		eval "$(ssh-agent -s)"
		ssh-add ~/.ssh/id_ed25519
		echo "ssh key added"
	fi
	## Wakatime
	rm -r -f ~/.wakatime.cfg
	ln -s ~/dotFiles/backup/wakatime.cfg ~/.wakatime.cfg
	## Wallpapers
	rm -r -f ~/Pictures/wallpapers
	ln -s ~/dotFiles/backup/wallpapers ~/Pictures/wallpapers
fi

## ZSH
rm -r -f ~/.zshrc
ln -s ~/dotFiles/config/zsh/.zshrc ~/.zshrc

## Develop
$ASK || ( echo -n "Do you need all develop software? [Y/n]" && read )
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	cat ~/dotFiles/apps/development >> ~/dotFiles/app.temp
fi

## Doom
$ASK || ( echo -n "Do you need Doom Emacs? [Y/n]" && read )
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
  if [[ ! $(~/.emacs.d/bin/doom info 2> /dev/null) ]]
  then
 	  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d 1> /dev/null || exit 1
	  ~/.emacs.d/bin/doom install --install --no-fonts 1> /dev/null || exit 1
  fi
	cat ~/dotFiles/apps/emacs >> ~/dotFiles/app.temp
fi

## Files
$ASK || ( echo -n "Do you need file system software? [Y/n]" && read )
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	cat ~/dotFiles/apps/files >> ~/dotFiles/app.temp
fi

## Games
$ASK || ( echo -n "Do you need all gaiming software? [Y/n]" && read )
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	cat ~/dotFiles/apps/games >> ~/dotFiles/app.temp
fi

## Xmonad
$ASK || ( echo -n "Do you want Xmonad? [Y/n]" && read )
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	echo "Copying all desktop settings..."
	echo "--- Alacritty"
	rm -r -f ~/.config/alacritty
	ln -s ~/dotFiles/config/alacritty ~/.config/alacritty
	echo "--- Conky"
	rm -r -f ~/.config/conky
	ln -s ~/dotFiles/config/conky ~/.config/conky
	echo "--- Doom Emacs"
	rm -r -f ~/.config/doom
	ln -s ~/dotFiles/config/doom ~/.config/doom
	echo "--- Dunts"
	rm -r -f ~/.config/dunst
	ln -s ~/dotFiles/config/dunst ~/.config/dunst
	echo "--- Nitrogen"
	rm -r -f ~/.config/nitrogen
	ln -s ~/dotFiles/config/nitrogen ~/.config/nitrogen
	echo "--- Picom"
	rm -r -f ~/.config/picom
	ln -s ~/dotFiles/config/picom ~/.config/picom
	echo "--- Rofi"
	rm -r -f ~/.config/rofi
	ln -s ~/dotFiles/config/rofi ~/.config/rofi
	echo "--- Xmobar"
	rm -r -f ~/.config/xmobar
	ln -s ~/dotFiles/config/xmobar ~/.config/xmobar
	echo "--- Xmonad"
	rm -r -f ~/.xmonad
	ln -s ~/dotFiles/config/xmonad ~/.xmonad
  ### Add programs
	cat ~/dotFiles/apps/xmonad >> ~/dotFiles/app.temp
fi

## Others
echo "Run"
echo "	cat ~/dotFiles/apps/others"
echo "for information about what other apps will be installed"
$ASK || ( echo -n "Do you this other apps? [Y/n]" && read )
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	cat ~/dotFiles/apps/others >> ~/dotFiles/app.temp
fi

## Install paru only if it doesn't exist in path
if [[ ! $(which paru 2> /dev/null) ]]
then
	sudo rm -r /home/andres/.cache/paru
	git clone https://aur.archlinux.org/paru.git ~/.cache/paru
	cd ~/.cache/paru/ || exit 1
	makepkg -si
	cd ~/ || exit 1
	sudo rm -r /home/andres/.cache/paru
fi

# Final instalation
echo "Inastalling all nedeed software..."
sudo paru -Syu --needed --noconfirm $( cat ~/dotFiles/apps/system ) $( cat ~/dotFiles/app.temp ) 1> /dev/null || exit 1

rm -r ~/dotFiles/app.temp
echo "Instalation Complete!"
