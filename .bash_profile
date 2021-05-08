# EXPORTS
export TERM='xterm-256color'
export HISTCONTROL=ignoredups:erasedups
export EDITOR='nvim'

# To work java jwt in any window manager
export _JAVA_AWT_WM_NONREPARENTING=1

[ -s "$HOME/.nvm/nvm.sh" ]          && \. "$HOME/.nvm/nvm.sh"
[ -s "$HOME/.nvm/bash_completion" ] && \. "$HOME/.nvm/bash_completion"


[[ -f ~/.bashrc ]] && . ~/.bashrc
