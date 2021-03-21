#!/bin/sh

# ln -s /path/to/playground/zshrc ~/.zshrc

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(zsh-autosuggestions zsh-syntax-highlighting colored-man-pages)

source $ZSH/oh-my-zsh.sh

export EDITOR='emacsclient --quiet -nw'
export VISUAL=$EDITOR

# Colors
export TERM=xterm-256color

# aliases
alias ls='logo-ls -lgG --human-readable --git-status --time-style RFC822'
alias ll='ls -lh'
alias la='ls -lah'
alias -s pdf='evince'
alias e=$EDITOR
alias watch='watch -tn 1'
alias log='tail -f'
alias diff='diff --color=auto'
alias pacman='pacman --color=always'
alias aur='paru'

# Golang
export GOPATH=$HOME/info/go
export GO111MODULE=on

export PATH=$HOME/.npm/bin:$GOPATH/bin:/usr/bin/vendor_perl:$HOME/info/deployer_shell_script_alias:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/var/lib/snapd/snap/bin

# NodeJS/NPM
npm config set prefix ~/.npm

unsetopt share_history

# less
export LESSOPEN="| src-hilite-lesspipe.sh %s"
export LESS=" -R "
alias less='less --long-prompt --LINE-NUMBERS --HILITE-SEARCH --ignore-case -J --underline-special --SILENT'
alias more='less'
alias cat="src-hilite-lesspipe.sh $1"
alias nano="nano -l"

# Base16 shell theme
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

# Disable paste animation
zstyle ':bracketed-paste-magic' active-widgets '.self-*'
