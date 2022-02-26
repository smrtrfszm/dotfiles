[[ $- != *i* ]] && return

PROMPT='%B%F{cyan}%~ %(?.%F{yellow}.%F{red})❱%f%b '

bindkey -v
KEYTIMEOUT=1

HISTFILE="$HOME/.histfile"
HISTSIZE=1000
SAVEHIST=1000

unsetopt beep
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS

zstyle :compinstall filename '/home/smrtrfszm/.zshrc'
autoload -Uz compinit
compinit

alias ls="exa --color=always --group-directories-first"
alias grep="grep --color=auto"
alias vim="nvim"
alias :q="exit"
alias copy="xclip -selection clipboard"
alias lg="lazygit"
alias cg="lazygit -w $HOME -g $HOME/configs"
alias python="python3.10"
alias config='git --git-dir=$HOME/configs --work-tree=$HOME'

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^n" down-line-or-search
bindkey "^p" up-line-or-search
bindkey "^j" vi-cmd-mode

function zle-line-init zle-keymap-select {
    case "$KEYMAP" in
        vicmd) echo -ne "\e[2 q" ;;
        viins|main) echo -ne "\e[6 q" ;;
    esac
}

zle -N zle-line-init
zle -N zle-keymap-select

source "$HOME/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
