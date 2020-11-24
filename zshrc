#!/bin/sh

# ln -s /path/to/playground/zshrc ~/.zshrc

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(zsh-autosuggestions zsh-syntax-highlighting colored-man-pages)

source $ZSH/oh-my-zsh.sh

export EDITOR='emacs --no-window-system --no-site-lisp --no-desktop --no-splash --no-site-file --no-init-file'
export VISUAL=$EDITOR

# Colors
export TERM=xterm-256color

# aliases
alias ll='ls -lh'
alias la='ls -lah'
alias -s pdf='evince'
alias e=$EDITOR
alias watch='watch -tn 1'
alias log='tail -f'
alias pacman='pacman --color=always'
alias diff='diff --color=auto'

# NodeJS/NPM
npm config set prefix ~/.npm
export PATH=$HOME/.npm/bin:$PATH

# Golang
export GOPATH=$HOME/info/go
export PATH=$PATH:$GOPATH/bin
export GO111MODULE=on

export PATH=$PATH:/usr/bin/vendor_perl
export PATH=$PATH:~/info/deployer_shell_script_alias

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
