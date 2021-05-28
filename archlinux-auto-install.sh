#!/bin/sh

# Start
cd $HOME

# Directories
mkdir $HOME/info/
export INFO=$HOME/info/

# Uncomment Misc Options in /etc/pacman.conf
# Color
# TotalDownload
# CheckSpace
# ILoveCandy

# System update
sudo pacman -Syu

# Update mirrorlist
sudo pacman -S reflector
sudo reflector --age 6 --latest 10 --fastest 10 --threads 10 --sort rate --protocol https --save /etc/pacman.d/mirrorlist

# Programs
sudo pacman -S \
  linux-hardened \
  linux-zen \
  linux-lts \
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
  keepassxc \
  android-tools \
  evince \
  filezilla \
  gparted \
  httpie \
  redshift \
  source-highlight \
  time \
  libreoffice-fresh \
  libreoffice-fresh-fr \
  python-pip \
  net-tools \
  dia \
  transmission-gtk \
  redshift \
  graphviz \
  docker \
  docker-compose \
  gnu-netcat \
  gnome-system-monitor \
  nano-syntax-highlighting \
  pm2 \
  npm-check-updates \
  yarn \
  typescript \
  simplescreenrecorder \
  fwupd \
  strace \
  speedtest-cli \
  yq \
  cloc \
  powertop \
  libqalculate \
  conky \
  obs-studio \
  tar \
  unzip \
  gzip \
  file-roller \
  screen \
  cowfortune \
  cowsay \
  lolcat \
  fd \
  the_silver_searcher \
  ripgrep \
  discord \
  sl \
  ntp \
  arandr \
  cmatrix \
  sof-firmware \
  alsa-ucm-conf \
  peek \
  hwinfo \
  cmake \
  procinfo \
  virtualbox-host-dkms \
  remmina \
  fprintd \
  arch-audit \
  fzf \
  networkmanager-openvpn \
  noto-fonts \
  xdg-utils \
  feh \

# Edit /etc/default/grub and set these
# GRUB_DISABLE_SUBMENU=y
# GRUB_DEFAULT=saved
# GRUB_SAVEDEFAULT=true

# Apply GRUB changes through
sudo grub-mkconfig -o /boot/grub/grub.cfg

sudo systemctl start ntpd.service
sudo systemctl enable ntpd.service

## Firmware
fwupdmgr get-devices
fwupdmgr refresh
fwupdmgr get-updates
fwupdmgr update

## RSA keys
ssh-keygen -t ed25519

# YaY
# git clone https://aur.archlinux.org/yay.git $INFO/yay
# cd $$INFO/yay
# makepkg -si
# cd $HOME

# Paru
git clone https://aur.archlinux.org/paru.git $$INFO/paru
cd $INFO/paru
makepkg -si
cd $HOME

paru -S \
  mongodb-bin \
  mongodb-tools-bin \
  postman-bin \
  robo3t-bin \
  multimarkdown \
  popcorntime-bin \
  apache-tools \
  pipes.sh \
  screenfetch \
  nerd-fonts-source-code-pro \
  logo-ls \
  arch-silence-grub-theme-git \
  icdiff \
  papirus-icon-theme \
  slack-desktop \
  downgrade \
  cava \
  hibernator \
  update-grub \
  virtualbox-ext-oracle \
  mugshot \
  influxdb-cli \
  mongodb-shell \
  etcher-bin \
  ttf-symbola \
  mononoki \
  xarchiver \

# TLP
sudo systemctl enable tlp.service
sudo tlp start

# Virtualbox
sudo modprobe vboxdrv

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
  yarn \
  pnpm \
  @microsoft/rush \
  ts-node-dev

# Docker
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker

# Golang
export GOPATH=$INFO/go
export GO111MODULE=on
go get golang.org/x/tools/cmd/...
go get github.com/rogpeppe/godef
go get -u github.com/isacikgoz/tldr

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
# git config --global user.email ""
# git config --global user.name ""

# git icdiff

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

# Ricing

paru -Ss \
  gtk-engine-murrine \
  gtk-engines \
  ulauncher

# Theme
git clone https://github.com/vinceliuice/Qogir-theme.git $INFO
cd $INFO/Qogir-theme
./install.sh
# Set theme Qogir-manjaro-win-dark

# ULauncher
git clone https://github.com/levonhart/materia-dark-ulauncher ~/.config/ulauncher/user-themes/materia-dark-ulauncher
# Extensions:
# https://github.com/henriqueutsch/exec-terminal
# https://ext.ulauncher.io/-/github-hippo-o-matic-ulauncher-qalc
# https://ext.ulauncher.io/-/github-fsevenm-ulauncher-uuid

# Terminal base16 theme
git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
# base16_<TAB>_<TAB
base16_helios

# Backup
#
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

# Remove all orphaned packages (installed packages that are no longer used/needed)
sudo pacman -Rns $(pacman -Qtdq)

# Finger print
fprintd-list $USER
fprintd-enroll
fprintd-verify
fprintd-list $USER
# Add "auth sufficient pam_fprintd.so" at the top of each file
# /etc/pam.d/{system-local-login,login,sudo,lightdm-*,polkit-1}
