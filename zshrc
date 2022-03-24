#!/bin/sh

# ln -s /path/to/playground/zshrc ~/.zshrc

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(
    zsh-autosuggestions
    zsh-syntax-highlighting
    colored-man-pages
)

source $ZSH/oh-my-zsh.sh

export EDITOR='emacsclient --quiet -nw'
export VISUAL=$EDITOR

# Colors
export TERM=xterm-256color

# Aliases
alias ls='logo-ls -l --human-readable --git-status --time-style RFC822'
alias ll='ls -lh'
alias l='ll'
alias la='ls -lah'
alias -s pdf='evince'
alias e=$EDITOR
alias watch='watch -tn 1'
alias log='tail -f'
alias diff='diff --color=auto'
alias icdiff='icdiff --highlight --line-numbers'
alias pacman='pacman --color=always'
alias aur='paru --color=always'
alias news='aur --show -w -w'
alias open='xdg-open'
alias tree='tree -C'
alias cp='cp --verbose --interactive'
alias mv='mv --verbose --interactive'
alias rm='rm --verbose'
alias grep='grep --line-number --color=always'

# less
export LESSOPEN="| src-hilite-lesspipe.sh %s"
export LESS=" -R "
alias less='less --long-prompt --LINE-NUMBERS --HILITE-SEARCH --ignore-case -J --underline-special --SILENT'
alias more='less'
alias cat="src-hilite-lesspipe.sh $1"
alias nano="nano -l"

# Golang
export GOPATH=$HOME/info/go
export GO111MODULE=on

export PATH=$HOME/.npm/bin:$GOPATH/bin:/usr/bin/vendor_perl:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/var/lib/snapd/snap/bin

# NPM auto completion
if type complete &>/dev/null; then
  _npm_completion () {
    local words cword
    if type _get_comp_words_by_ref &>/dev/null; then
      _get_comp_words_by_ref -n = -n @ -n : -w words -i cword
    else
      cword="$COMP_CWORD"
      words=("${COMP_WORDS[@]}")
    fi

    local si="$IFS"
    if ! IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${words[@]}" \
                           2>/dev/null)); then
      local ret=$?
      IFS="$si"
      return $ret
    fi
    IFS="$si"
    if type __ltrim_colon_completions &>/dev/null; then
      __ltrim_colon_completions "${words[cword]}"
    fi
  }
  complete -o default -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    local si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    if ! IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)); then

      local ret=$?
      IFS="$si"
      return $ret
    fi
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi

unsetopt share_history

# Updates pacman, AUR and NPM packages
update(){
    sudo pacman -Syu
    aur -Syu
    npm update -g
    # pnpm update -g
}

loop(){
		for i in {1..$1}; do eval $2; done
}

fuzzy-install(){
    pacman -Slq | fzf --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S
}

fuzzy-remove(){
    pacman -Qq | fzf --multi --preview 'pacman -Qi {1}' | xargs -ro sudo pacman -Rns
}

# Base16 shell theme
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

# Disable paste animation
zstyle ':bracketed-paste-magic' active-widgets '.self-*'

# Emacs vterm
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true

# Rush auto completion
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
source ~/.rush_auto_completion

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
