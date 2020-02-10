#!/bin/sh

# Start
cd $HOME

# Directories
mkdir $HOME/info/
rmdir $HOME/Public $HOME/Modèles

# System update
sudo pacman -Syu

# LTS Kernel
sudo pacman -S linux-lts
sudo pacman install linux-lts-headers
sudo pacman -Rs linux

# Programs
sudo pacman -S git tk vlc chromium emacs terminator tlp tlp-rdw zsh tree source-highlight jq ack gparted go nodejs ack redis wine texlive-core npm postfix

# YaY
git clone https://aur.archlinux.org/yay.git $HOME/info/yay
cd $HOME/info/yay
makepkg -si
cd $HOME

yay -S mongodb-bin mongodb-tools-bin postman-bin robo3t-bin keepassx

# TLP
sudo tlp start

# Oh-My-ZSH
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo chsh -s /bin/zsh

# Node
sudo npm install -g c8 eslint flamebearer npm-check-updates typescript tslint yarn

# Golang
go get golang.org/x/tools/cmd/...
go get github.com/rogpeppe/godef

# Cleanup
sudo pacman -Rns $(pacman -Qtdq)

# Backup
sudo rsync -aAXvP --delete --exclude=/dev/* --exclude=/proc/* --exclude=/sys/* --exclude=/tmp/* --exclude=/run/* --exclude=/mnt/* --exclude=/media/* --exclude=/lost+found / /mnt/backup

# Restore backup
# rsync -aAXv --delete --exclude="lost+found" /mnt/usb/ /mnt/system/