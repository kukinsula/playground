#!/bin/sh

# ln -s /path/to/playground/zshrc ~/.zshrc

# git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
# git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
# git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(zsh-autosuggestions zsh-syntax-highlighting colored-man-pages)

source $ZSH/oh-my-zsh.sh

export EDITOR='emacs --no-window-system --no-site-lisp --no-desktop --no-splash --no-site-file --no-init-file config/dev.json'
export VISUAL=$EDITOR

# Colors
export TERM=xterm-256color

# aliases
alias ll='ls -l'
alias la='ls -la'
alias -s pdf='evince'
alias e=$EDITOR
alias watch='watch -tn 1'
alias seqSL='seq 10 | xargs -Iz sl'
alias log='tail -f'
alias pacman='pacman --color=always'

# Golang
export GOPATH=$HOME/info/go
export PATH=$PATH:$GOPATH/bin
export GO111MODULE=on

export PATH=$PATH:/usr/bin/vendor_perl
export PATH=$PATH:~/info/deployer_shell_script_alias

unsetopt share_history

# less
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS='-R '

# Base16 shell theme
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"
