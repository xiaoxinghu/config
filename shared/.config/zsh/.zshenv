# -*- mode: shell-script -*-
# Environment variables sourced by all zsh instances (login, interactive,
# scripts). Bootstrapped from ~/.zshenv.

# PATH — Homebrew (Apple Silicon / Intel)
if [[ -d "/opt/homebrew/bin" ]]; then
    export PATH="/opt/homebrew/bin:$PATH"
elif [[ -d "/usr/local/bin" ]]; then
    export PATH="/usr/local/bin:$PATH"
fi

# Local user binaries
export PATH="$HOME/.local/bin:$PATH"

# Bun
export PATH="$HOME/.bun/bin:$PATH"

# Rust / Cargo
[[ -d "$HOME/.cargo" ]] && . "$HOME/.cargo/env"
