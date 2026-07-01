# -*- mode: shell-script -*-
# Login shell configuration

eval "$(/opt/homebrew/bin/brew shellenv)"
export BUN_INSTALL="$HOME/.bun"
export PATH="$HOME/.local/bin:$BUN_INSTALL/bin:$PATH"
