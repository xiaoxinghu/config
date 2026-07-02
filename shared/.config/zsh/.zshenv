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

# mise shims — make mise-managed tools (rg, node, nvim, …) available to
# non-interactive shells and GUI apps (e.g. Emacs via exec-path-from-shell)
# that don't run `mise activate`. Interactive shells still prefer the real
# tool dirs that `mise activate` prepends in .zshrc.
export PATH="${XDG_DATA_HOME:-$HOME/.local/share}/mise/shims:$PATH"

# Bun
export PATH="$HOME/.bun/bin:$PATH"

# Rust / Cargo
[[ -d "$HOME/.cargo" ]] && . "$HOME/.cargo/env"
