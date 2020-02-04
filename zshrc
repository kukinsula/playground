export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=()

source $ZSH/oh-my-zsh.sh

export EDITOR='emacsclient --create-frame --quiet -nwQ '
export VISUAL='emacsclient --create-frame --quiet -nwQ '

# Colors
export TERM=xterm-256color

# aliases
alias la="ls -a"
alias lla='ls -la'
alias -s pdf="evince "
alias e="emacsclient --create-frame --quiet -nw "
alias watch="watch -tn 1"
alias seqSL="seq 10 | xargs -Iz sl"

export GOPATH=$HOME/info/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/bin/vendor_perl

unsetopt share_history

# less
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS='-R '
