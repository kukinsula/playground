#!/bin/sh

# Setup
mkdir $HOME/info/
rmdir Public Modèles


# Update
sudo apt-get update
sudo apt-get dist-upgrade


# Programs
sudo apt-get install -y ubuntu-restricted-extras git gitk vlc chromium-browser emacs terminator tlp tlp-rdw zsh tree source-highlight jq build-essential ack-grep


# Terminator as a keyboard shortcut
# terminator --geometry=600x845+1000+0


# TLP
sudo tlp start


# Oh-My-ZSH
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo chsh -s /bin/zsh


# Golang
wget https://storage.googleapis.com/golang/go1.8.3.linux-amd64.tar.gz
sudo tar -C /usr/local -xzvf go1.8.3.linux-amd64.tar.gz
mkdir $HOME/info/go


# Nodejs
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install -y nodejs


# Cleanup
sudo apt-get autoclean
sudo apt-get autoremove


# Locate
sudo updatedb


# SSH
ssh-keygen
