# Set XDG Base Directory paths
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Set ZDOTDIR to use XDG config directory
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Environment variables (sourced by all zsh instances)

# PATH configuration
# Homebrew (macOS Intel and Apple Silicon)
if [[ -d "/opt/homebrew/bin" ]]; then
    export PATH="/opt/homebrew/bin:$PATH"
elif [[ -d "/usr/local/bin" ]]; then
    export PATH="/usr/local/bin:$PATH"
fi

# Local user binaries
export PATH="$HOME/.local/bin:$PATH"

# Bun
export PATH="$HOME/.bun/bin:$PATH"
. "$HOME/.cargo/env"
