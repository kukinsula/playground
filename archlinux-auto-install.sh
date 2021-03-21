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

# Uncomment Misc Options in /etc/pacman.conf
# Color
# TotalDownload
# CheckSpace
# ILoveCandy

# Programs
sudo pacman -S \
  git \
  tig \
  tk \
  vlc \
  chromium \
  emacs \
  terminator \
  tlp \
  tlp-rdw \
  zsh \
  tree \
  jq \
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
  redshift \
  source-highlight \
  time \
  libreoffice-still \
  libreoffice-still-fr \
  python-pip \
  net-tools \
  etcher \
  dia \
  transmission-gtk \
  redshift \
  graphviz \
  docker \
  docker-compose \
  netcat \
  gnome-system-monitor \
  nano-syntax-highlighting \
  pm2 \
  npm-check-updates \
  yarn \
  typescript \
  simplescreenrecorder \
  fwupd \
  map \
  trace \
  loc \
  peedtest-cli \
  yq \
	cloc

## Firmware
fwupdmgr get-devices
fwupdmgr refresh
fwupdmgr get-updates
fwupdmgr update

## RSA keys
ssh-keygen -b 4096

# YaY
# git clone https://aur.archlinux.org/yay.git $HOME/info/yay
# cd $HOME/info/yay
# makepkg -si
# cd $HOME

# Paru
git clone https://aur.archlinux.org/paru.git $HOME/info/paru
cd $HOME/info/paru
makepkg -si
cd $HOME

paru -S \
  mongodb-bin \
  mongodb-tools-bin \
  postman-bin \
  robo3t-bin \
  rambox-bin \
  usb-creator \
  multimarkdown \
	nodejs-tern \
	popcorntime-bin \
	apache-tools \
	pipes.sh

# TLP
sudo tlp start

# Oh-My-ZSH
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo chsh -s /bin/zsh

git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions

# Node
npm install -g \
  c8 \
  flamebearer \
  node-gyp \
  npm-check \
  prettier \
  semver \
  tern \
  tslint \
  typescript \
  yarn

# Golang1
go get golang.org/x/tools/cmd/...
go get github.com/rogpeppe/godef

# Cleanup
sudo pacman -Rns $(pacman -Qtdq)

# /etc/hosts
sudo curl https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts --output /etc/hosts

# Python
sudo pip install \
  black \
  rope \
  jedi \
  flake8 \
  importmagic \
  autopep8 \
  yapf

# Git
git config --global user.email ""
git config --global user.name ""

# Source highlight

## YAML
sudo cat >> /usr/share/source-highlight/yaml.lang <<EOL
# source-highlight's language definition file for YAML

include "script_comment.lang"
include "number.lang"

keyword = "true|false|null"

section start '^---'
(symbol,name,symbol) = `(^[[:blank:]-]*)([[:alnum:]_]+)(:)`
symbol = '^[[:blank:]]*-'

# TODO:
#   - hredoc
#   - alias indicators

string delim "\"" "\"" escape "\\"
string delim "'" "'"  escape "\\"
EOL

## Add languages
sudo echo "yml = yaml.lang" >> /usr/share/source-highlight/lang.map
sudo echo "yaml = yaml.lang" >> /usr/share/source-highlight/lang.map
sudo echo "ts = javascript.lang" >> /usr/share/source-highlight/lang.map
sudo echo "md = sh.lang" >> /usr/share/source-highlight/lang.map

# Nanorc
echo "include /usr/share/nano-syntax-highlighting/*.nanorc" >> ~/.nanorc

# Base16 theme
git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
# base16_<TAB>_<TAB
base16_helios

# Backup
# sudo rsync -aAXvP \
#   --delete \
#   --exclude=/dev/* \
#   --exclude=/proc/* \
#   --exclude=/sys/* \
#   --exclude=/tmp/* \
#   --exclude=/run/* \
#   --exclude=/mnt/* \
#   --exclude=/media/* \
#   --exclude=/lost+found \
#   / \
#   /mnt/backup

# Restore backup
# rsync -aAXv --delete --exclude="lost+found" /mnt/usb/ /mnt/system/
