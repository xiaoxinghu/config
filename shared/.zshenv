# -*- mode: shell-script -*-
# Bootstrap — MUST live in $HOME.
# Zsh reads this before it knows ZDOTDIR, so it's the pointer that makes
# the rest of the config load from ~/.config/zsh.

# XDG Base Directory paths
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Send zsh to ~/.config/zsh for the remaining startup files
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Everything else lives under $ZDOTDIR (not auto-sourced, so source it here)
[[ -f "$ZDOTDIR/.zshenv" ]] && source "$ZDOTDIR/.zshenv"
