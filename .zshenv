export TERM="xterm-256color"
export EDITOR="nvim"

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

export LESSHISTFILE=-
export PNPM_HOME="$XDG_DATA_HOME/pnpm"
export GOPATH="$XDG_DATA_HOME/go"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export MYSQL_HISTFILE="$XDG_STATE_HOME/mysql_history"
export NODE_REPL_HISTORY="$XDG_STATE_HOME/node_repl_history"
export REDISCLI_HISTFILE="$XDG_STATE_HOME/rediscli_history"
export REDISCLI_RCFILE="$XDG_CONFIG_HOME/redis/redisclirc"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export XMONAD_DATA_DIR="$XDG_DATA_HOME/xmonad"
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME/xmonad"
export XMONAD_CACHE_DIR="$XDG_CACHE_HOME/xmonad"
export CUDA_CACHE_PATH="$XDG_CACHE_HOME/nv"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/startup.py"
export KUBECONFIG="$XDG_CONFIG_HOME/kube/config"
export MINIKUBE_HOME="$XDG_DATA_HOME/minikube"
export _JAVA_AWT_WM_NONREPARENTING=1
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export PSQL_HISTORY="$XDG_STATE_HOME/psql_history"

path+=("$HOME/.local/bin")
path+=("$CARGO_HOME/bin")
path+=("$GOPATH/bin")
path+=("$PNPM_HOME")
