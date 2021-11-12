#!/usr/bin/env bash

## Clean temp file
echo "" > ~/dotFiles/app.temp

# Nedded
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
rm -r -f ~/.gitconfig
ln -s ~/dotFiles/config/git/gitconfig ~/.gitconfig
echo -n "Do you have your backup folder?(Wakatime and Wallpapers)[Y/n]" && read
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	## SSH
	rm -r -f ~/.ssh
	mkdir ~/.ssh
	cp ~/dotFiles/backup/ssh/* ~/.ssh/
	if [[ -e "$HOME/.ssh/id_ed25519" ]]; then
		chmod -R 700 ~/.ssh
		eval "$(ssh-agent -s)"
		ssh-add ~/.ssh/id_ed25519
		echo "shh key added"
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
# Opcional
## Artix
echo -n "Are you running Artix? [Y/n]" && read
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	echo "artix-archlinux-support" >> ~/dotFiles/app.temp
	echo -n "Are you runnit on OpenRC? [Y/n]" && read
	echo "Root is needed to write on /etc/pacman.conf"
	sudo mv /etc/pacman-artix.conf /etc/pacman.conf
	if [[ ! $REPLY =~ ^[Nn]$ ]]
	then
		 cat ~/dotFiles/apps/openrc >> ~/dotFiles/app.temp
		echo " * remember to add all needed services ;)"
	fi
else
	sudo rm /etc/pacman-artix.conf
fi
## Games
echo "Run"
echo "	cat ~/dotFiles/apps/game/amd"
echo "for information about what other apps will be installed"
echo -n "Do you need all gaiming software? [Y/n]" && read
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	cat ~/dotFiles/apps/game/amd >> ~/dotFiles/app.temp
fi
## Others
echo -n "Do you this other apps? [Y/n]" && read
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	cat ~/dotFiles/apps/others >> ~/dotFiles/app.temp
fi
## Xmonad
echo -n "Do you want Xmonad? [Y/n]" && read
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
### Alacritty
	rm -r -f ~/.config/alacritty
	ln -s ~/dotFiles/config/alacritty ~/.config/alacritty
### Conky
	rm -r -f ~/.config/conky
	ln -s ~/dotFiles/config/conky ~/.config/conky
### Doom Emacs
	rm -r -f ~/.config/doom
	ln -s ~/dotFiles/config/doom ~/.config/doom
### Dunts
	rm -r -f ~/.config/dunst
	ln -s ~/dotFiles/config/dunst ~/.config/dunst
### Nitrogen
	rm -r -f ~/.config/nitrogen
	ln -s ~/dotFiles/config/nitrogen ~/.config/nitrogen
### Picom
	rm -r -f ~/.config/picom
	ln -s ~/dotFiles/config/picom ~/.config/picom
### Rofi
	rm -r -f ~/.config/rofi
	ln -s ~/dotFiles/config/rofi ~/.config/rofi
### Xmobar
	rm -r -f ~/.config/xmobar
	ln -s ~/dotFiles/config/xmobar ~/.config/xmobar
### Xmonad
	rm -r -f ~/.xmonad
	ln -s ~/dotFiles/config/xmonad ~/.xmonad
### Add programs
	cat ~/dotFiles/apps/xmonad >> ~/dotFiles/app.temp
fi
# Final instalation
echo "Inastalling all nedeed software..."
sudo pacman -Syu --needed --noconfirm $( cat ~/dotFiles/apps/default ) $( cat ~/dotFiles/app.temp )
## Paru
echo -n "Do you need Paru and all Paru software? [Y/n]" && read
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	git clone https://aur.archlinux.org/paru.git ~/.cache/paru
	cd ~/.cache/paru
	makepkg -si
	paru -S --needed --noconfirm $( cat ~/dotFiles/apps/aur )
fi
## Doom
echo -n "Do you need Doom Emacs? [Y/n]" && read
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
	git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
	~/.emacs.d/bin/doom install
	paru -S --needed --noconfirm $( cat ~/dotFiles/apps/emacs )
fi
rm -r ~/dotFiles/app.temp
echo "Instalation Complete!"
