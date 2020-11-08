
# Set vi mode
set -o vi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Path
[ -d "$HOME/scripts" ]    && PATH="$HOME/scripts/:$PATH"
[ -d "$HOME/local/.bin" ] && PATH="$HOME/local/.bin/:$PATH"

# shopt
shopt -s histappend
shopt -s checkwinsize

PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '


## ALIASES ##
# change 'ls' to 'exa'
alias ls='exa --color=always --group-directories-first'

# colorize grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias vim='nvim'
alias python='python3.8'

# alias for bare config git repo
alias config='git --git-dir=$HOME/configs --work-tree=$HOME'

# vim exit in bash
alias ':q'='exit'


