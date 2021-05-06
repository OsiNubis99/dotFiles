## Create Dirs
mkdir -p ~/.config
mkdir -p ~/.local/share
mkdir -p ~/Documents
mkdir -p ~/Downloads
mkdir -p ~/Music
mkdir -p ~/Pictures
mkdir -p ~/Public
mkdir -p ~/Videos

## Alacritty
rm -r -f ~/.config/alacritty
ln -s ~/dotFiles/config/alacritty ~/.config/alacritty

## Donts
rm -r -f ~/.config/dunst
ln -s ~/dotFiles/config/dunst ~/.config/dunst

## Fontconfig
rm -r -f ~/.local/share/fonts
ln -s ~/dotFiles/config/fonts ~/.local/share/fonts
rm -r -f ~/.config/fontconfig
ln -s ~/dotFiles/config/fontconfig ~/.config/fontconfig

## Git
rm -r -f ~/.gitconfig
ln -s ~/dotFiles/config/git/gitconfig ~/.gitconfig

## Nitrogen
rm -r -f ~/.config/nitrogen
ln -s ~/dotFiles/config/nitrogen ~/.config/nitrogen

## Picom
rm -r -f ~/.config/picom
ln -s ~/dotFiles/config/picom ~/.config/picom

## SSH
rm -r -f ~/.ssh
ln -s ~/dotFiles/backup/ssh ~/.ssh
if [[ -e ".ssh/id_ed25519" ]]; then
	ssh-add ~/.ssh/id_ed25519
	cat .ssh/id_ed25519.pub
fi

## Xmobar
rm -r -f ~/.config/xmobar
ln -s ~/dotFiles/config/xmobar ~/.config/xmobar

## Xmonad
rm -r -f ~/.xmonad
ln -s ~/dotFiles/config/xmonad ~/.xmonad

## Wallpapers
rm -r -f ~/Pictures/wallpapers
ln -s ~/dotFiles/backup/wallpapers ~/Pictures/wallpapers

## Wifi Connections
# sudo rm -r -f /etc/NetworkManager/system-connections
# sudo ln -s ~/dotFiles/backup/wifi/ /etc/NetworkManager/system-connections

## ZSH
rm -r -f ~/.zshrc
ln -s ~/dotFiles/config/zsh/.zshrc ~/.zshrcln -s ~/dotFiles/config/zsh/.zshrc ~/.zshrc

## Response
echo "You must configure manually (./configs can help): "
echo "lightdm"
echo "system-connections"
echo "X11"
