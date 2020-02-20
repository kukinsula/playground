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
sudo pacman -S \
  git \
  tk \
  vlc \
  chromium \
  emacs \
  terminator \
  tlp \
  tlp-rdw \
  zsh \
  tree \
  source-highlight \
  jq \
  ack \
  gparted \
  go \
  nodejs \
  ack \
  redis \
  wine \
  texlive-core \
  npm \
  postfix \
  keepassxc \
  android-tools \
  evince \
  filezilla \
  gparted \
  httpie \
  jq \
  redshift \
  source-highlight \
  time \
  libreoffice-still \
  libreoffice-still-fr \
  python-pip

# YaY
git clone https://aur.archlinux.org/yay.git $HOME/info/yay
cd $HOME/info/yay
makepkg -si
cd $HOME

yay -S \
  mongodb-bin \
  mongodb-tools-bin \
  postman-bin \
  robo3t-bin \
  rambox-bin \
  usb-creator

# TLP
sudo tlp start

# Oh-My-ZSH
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo chsh -s /bin/zsh

# Node
sudo npm install -g \
  c8 \
  eslint \
  flamebearer \
  npm-check-updates \
  typescript \
  tslint \
  yarn \
  pm2

# Golang
go get golang.org/x/tools/cmd/...
go get github.com/rogpeppe/godef

# Cleanup
sudo pacman -Rns $(pacman -Qtdq)

# /etc/hosts
sudo curl https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts --output /etc/hosts

# Backup
sudo rsync -aAXvP --delete --exclude=/dev/* --exclude=/proc/* --exclude=/sys/* --exclude=/tmp/* --exclude=/run/* --exclude=/mnt/* --exclude=/media/* --exclude=/lost+found / /mnt/backup

# Restore backup
# rsync -aAXv --delete --exclude="lost+found" /mnt/usb/ /mnt/system/

# Python
sudo pip install black rope jedi flake8 importmagic autopep8 yapf
