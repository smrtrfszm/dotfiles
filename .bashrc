# Set vi mode
set -o vi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Path
[ -d "$HOME/scripts" ]    && PATH="$HOME/scripts/:$PATH"
[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin/:$PATH"
[ -d "$HOME/.yarn/bin" ]  && PATH="$HOME/.yarn/bin/:$PATH"
[ -d "$HOME/.cargo/bin" ] && PATH="$HOME/.cargo/bin/:$PATH"

# shopt
shopt -s histappend
shopt -s checkwinsize

PS1='\[\033[1;36m\]\w\[\033[1;33m\] > \[\033[0m\]'


## ALIASES ##
# change 'ls' to 'exa'
alias ls='exa --color=always --group-directories-first'

# colorize grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias vim='nvim'
alias python='python3.9'
alias lg='lazygit'
alias cg="lazygit -w $HOME -g $HOME/configs"

# alias for bare config git repo
alias config='git --git-dir=$HOME/configs --work-tree=$HOME'

# vim exit in bash
alias ':q'='exit'

alias copy="xclip -selection clipboard"
