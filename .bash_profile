# EXPORTS
export TERM='xterm-256color'
export HISTCONTROL=ignoredups:erasedups
export EDITOR='nvim'

export GNUMAKEFLAGS="-j14"

export GOPATH="$HOME/dev/go"
export PNPM_HOME="/home/smrtrfszm/.local/share/pnpm"

# To work java jwt in any window manager
export _JAVA_AWT_WM_NONREPARENTING=1

[ -s "$XDG_CONFIG_HOME/nvm/nvm.sh" ]          && \. "$XDG_CONFIG_HOME/nvm/nvm.sh"
[ -s "$XDG_CONFIG_HOME/nvm/bash_completion" ] && \. "$XDG_CONFIG_HOME/nvm/bash_completion"


[[ -f ~/.bashrc ]] && . ~/.bashrc
