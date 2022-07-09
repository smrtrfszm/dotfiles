[[ $- != *i* ]] && return

PROMPT='%B%F{cyan}%~ %(?.%F{yellow}.%F{red})❱%f%b '

bindkey -v
KEYTIMEOUT=1

HISTFILE="$XDG_STATE_HOME/zsh_history"
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
mkdir -p "$XDG_CACHE_HOME/zsh"
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION"

alias ls="exa --color=always --group-directories-first"
alias grep="grep --color=auto"
alias vim="nvim"
alias :q="exit"
alias copy="xclip -selection clipboard"
alias lg="lazygit"
alias cg="lazygit -w $HOME -g $HOME/.dotfiles"
alias config="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias python="python3.10"
alias kubectl="kubectl --cache-dir='$XDG_CACHE_HOME/kube'"
alias k="kubectl"

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^n" down-line-or-search
bindkey "^p" up-line-or-search
bindkey "^j" vi-cmd-mode
bindkey '^R' history-incremental-search-backward

function zle-line-init zle-keymap-select {
    case "$KEYMAP" in
        vicmd) echo -ne "\e[2 q" ;;
        viins|main) echo -ne "\e[6 q" ;;
    esac
}

zle -N zle-line-init
zle -N zle-keymap-select

source "$XDG_CONFIG_HOME/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

export GPG_TTY="$(tty)"
