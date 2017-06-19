# Path to your oh-my-zsh installation.
export ZSH=/home/kuk/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git golang)

# User configuration

export PATH="/bin:/sbin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/local/go/bin"

source $ZSH/oh-my-zsh.sh

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacs -nw '
else
  export EDITOR='emacs -nw '
fi

# Colors
export TERM=xterm-256color

# aliases
alias la="ls -a"
alias lla='ls -la'
alias -s pdf="evince "
alias e="emacs -nw "
alias update="sudo apt update && sudo apt dist-upgrade"
alias install="sudo apt install "
alias search="sudo apt search "

# Go
export GOPATH=$HOME/info/go
export PATH=$PATH:$GOPATH/bin

unsetopt share_history

# less
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS='-R '

# Shortcuts
export PLAY=$GOPATH/src/github.com/kukinsula/playground
export MONITORING=$GOPATH/src/github.com/kukinsula/monitoring
