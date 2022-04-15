export TERM="xterm-256color"
export EDITOR="nvim"

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

export LESSHISTFILE=-
export PNPM_HOME="$XDG_DATA_HOME/pnpm"
export GOPATH="$XDG_DATA_HOME/go"

path+=("$HOME/scripts")
path+=("$HOME/.local/bin")
path+=("$HOME/.cargo/bin")
path+=("$GOPATH/bin")
path+=("$PNPM_HOME")

if [ -n "$DESKTOP_SESSION" ]; then
	eval $(gnome-keyring-daemon --start)
	export SSH_AUTH_SOCK
fi
