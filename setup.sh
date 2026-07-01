#!/usr/bin/env bash
# Symlink dotfiles into $HOME with GNU stow.
# Detects the OS and stows the shared package plus the matching platform
# package (macos or linux), merged together. Idempotent.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$(uname -s)" in
  Darwin) PLATFORM="macos" ;;
  Linux)  PLATFORM="linux" ;;
  *) echo "Unsupported OS: $(uname -s)" >&2; exit 1 ;;
esac

if ! command -v stow >/dev/null 2>&1; then
  echo "GNU stow is not installed (brew install stow / apt install stow)." >&2
  exit 1
fi

pkgs=(shared)
[[ -d "$REPO/$PLATFORM" ]] && pkgs+=("$PLATFORM")

echo "Stowing: ${pkgs[*]} -> $HOME"
stow --dir="$REPO" --target="$HOME" --restow --verbose "${pkgs[@]}"
