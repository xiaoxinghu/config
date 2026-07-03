#!/usr/bin/env bash
# Bootstrap platform-bound packages on Linux.
# Detects the distro package manager and installs the things mise can't manage
# (the shell, git, tmux, pass, stow), then bootstraps mise itself. The
# cross-platform CLI tools come from mise (shared/.config/mise/config.toml) and
# the zsh plugins/prompt are handled by antidote from .zshrc. Idempotent.
set -euo pipefail

# Run a command as root when we aren't already.
run() { if [[ $EUID -eq 0 ]]; then "$@"; else sudo "$@"; fi; }

# Echo the first supported package manager found on PATH.
detect_pm() {
  for pm in apt-get dnf pacman zypper apk; do
    command -v "$pm" >/dev/null 2>&1 && { echo "$pm"; return; }
  done
  echo "No supported package manager found" >&2
  exit 1
}

# ----- system packages -----
install_system() {
  case "$(detect_pm)" in
    apt-get) run apt-get update -y
             run apt-get install -y zsh git tmux pass gnupg stow curl ca-certificates build-essential ;;
    dnf)     run dnf install -y zsh git tmux pass gnupg2 stow curl ca-certificates gcc make ;;
    pacman)  run pacman -Syu --needed --noconfirm zsh git tmux pass gnupg stow curl base-devel ;;
    zypper)  run zypper install -y zsh git tmux pass gpg2 stow curl gcc make ;;
    apk)     run apk add zsh git tmux pass gnupg stow curl ca-certificates build-base ;;
  esac
}

# ----- mise -----
install_mise() {
  command -v mise >/dev/null 2>&1 && return
  echo "Installing mise..."
  curl -fsSL https://mise.run | sh
}

install_system
install_mise
