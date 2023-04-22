#!/bin/sh

# ln -s /path/to/playground/zshrc ~/.zshrc

# Turnon zsh profiler
# zmodload zsh/zprof

ZSH_COMPDUMP="$ZSH_CACHE_DIR/.zcompdump"

# ZSH_THEME="robbyrussell"

autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats '%b '

ZSH_THEME="robbyrussell"
# PROMPT='%B%F{3}[%2d]%f %F{9}${vcs_info_msg_0_}%f%(?.%F{2}»%f.%F{203}❯%f) '

setopt NO_CASE_GLOB
unsetopt share_history

#

plugins=(
  zsh-autosuggestions
  # zsh-syntax-highlighting
  F-Sy-H
  colored-man-pages
)

zstyle ':omz:update' frequency 16
export ZSH=$HOME/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

# unsetopt share_history
setopt extendedglob

# Disable paste animation
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# ???
# autoload -Uz compinit
# for dump in ~/.zcompdump(N.mh+24); do
#   compinit
# done
# compinit -C

export EDITOR='emacsclient --quiet -nw'
export VISUAL=$EDITOR

# Colors
export TERM=xterm-256color

# Aliases

preexec(){ [ $1 != $2 ] && print -r "> $2" }

## ls
# alias ls='logo-ls -l --human-readable --git-status --time-style RFC822'
# alias ll='ls -lh'
# alias l='ll'
# alias la='ls -lah'
alias ls='exa --color always --binary --icons --modified --git'
alias ll='ls --long'
alias la='ll --all'
alias lll='ll'

alias -s pdf='evince'
alias e=$EDITOR
alias watch='watch -tn 1'
alias log='tail -f'
alias diff='diff --color=auto'
alias icdiff='icdiff --highlight --line-numbers'
alias open='xdg-open'
alias tree='tree -C'
alias cp='cp --verbose --interactive'
alias mv='mv --verbose --interactive'
alias rm='rm --verbose'
alias grep='grep --line-number --color=always'
alias df='duf'

# Arch
alias pacman='pacman --color=always'
alias aur='paru --color=always'
alias news='aur --show -w -w'

# export LESSOPEN="| src-hilite-lesspipe.sh %s"
# export LESS=" -R "
# alias less='less --long-prompt --LINE-NUMBERS --HILITE-SEARCH --ignore-case -J --underline-special --SILENT'
# alias more='less'
# alias nano="nano -l"
# alias cat="src-hilite-lesspipe.sh $1"
alias cat='bat'
alias less='bat'

# Golang
export GOPATH=$HOME/info/go
export GO111MODULE=on

export PATH=$HOME/.npm/bin:$GOPATH/bin:/usr/bin/vendor_perl:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/var/lib/snapd/snap/bin

# Updates pacman, AUR and NPM packages
update() {
    sudo pacman -Syu
    aur -Syu
    npm update -g
    # pnpm update -g
}

loop() {
		for i in {1..$1}; do eval $2; done
}

# Pacman helpers
fuzzy-install() {
    pacman -Slq | fzf --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S
}

fuzzy-remove() {
    pacman -Qq | fzf --multi --preview 'pacman -Qi {1}' | xargs -ro sudo pacman -Rns
}

# Base16 shell theme
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

# Emacs vterm
# vterm_printf(){
#     if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
#         # Tell tmux to pass the escape sequences through
#         printf "\ePtmux;\e\e]%s\007\e\\" "$1"
#     elif [ "${TERM%%-*}" = "screen" ]; then
#         # GNU screen (screen, screen-256color, screen-256color-bce)
#         printf "\eP\e]%s\007\e\\" "$1"
#     else
#         printf "\e]%s\e\\" "$1"
#     fi
# }

# Rush auto completion
# autoload -U +X compinit && compinit
# autoload -U +X bashcompinit && bashcompinit
# source ~/.rush_auto_completion

# bun completions
# [ -s "/home/kuk/.bun/_bun" ] && source "/home/kuk/.bun/_bun"

# Bun
export BUN_INSTALL="/home/kuk/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# McFly
# eval "$(mcfly init zsh)"
# export MCFLY_FUZZY=2
# export MCFLY_RESULTS=50
# export MCFLY_RESULTS_SORT=LAST_RUN

# NPM autocompletion for ZSH
# if type compdef &>/dev/null; then
#     _npm_completion() {
#         local si=$IFS

#         # if your npm command includes `install`
#         if [[ ${words} =~ 'install' ]] || [[ ${words} =~ 'i ' ]]; then
#             compadd -- $(COMP_CWORD=$((CURRENT-1)) \
#                 COMP_LINE=$BUFFER \
#                 COMP_POINT=0 \
#                 ls ~/.npm -- "${words[@]}" \
#                 2>/dev/null)

#         else
#             compadd -- $(COMP_CWORD=$((CURRENT-1)) \
#                 COMP_LINE=$BUFFER \
#                 COMP_POINT=0 \
#                 npm completion -- "${words[@]}" \
#                 2>/dev/null)
#         fi

#         IFS=$si
#     }
#     compdef _npm_completion npm
# elif type compctl &>/dev/null; then

#     _npm_completion () {
#         local cword line point words si
#         read -Ac words
#         read -cn cword
#         let cword-=1
#         read -l line
#         read -ln point
#         si="$IFS"
#         IFS=$'\n' reply=($(COMP_CWORD="$cword" \
#             COMP_LINE="$line" \
#             COMP_POINT="$point" \
#             npm completion -- "${words[@]}" \
#             2>/dev/null)) || return $?
#         IFS="$si"
#     }
#     compctl -K _npm_completion npm
# fi

# Turnoff zsh profiler
# zprof
