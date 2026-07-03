#!/usr/bin/env bash
# Set up this machine. Runs the platform bootstrap (native packages + mise),
# symlinks dotfiles into $HOME with GNU stow (shared + the matching platform
# package), then installs the cross-platform CLI tools with mise. Idempotent.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$(uname -s)" in
  Darwin) PLATFORM="macos" ;;
  Linux)  PLATFORM="linux" ;;
  *) echo "Unsupported OS: $(uname -s)" >&2; exit 1 ;;
esac

# ----- setup platform -----
if [[ -x "$REPO/$PLATFORM/setup.sh" ]]; then
  "$REPO/$PLATFORM/setup.sh"
fi

# ----- config -----
pkgs=(shared)
[[ -d "$REPO/$PLATFORM" ]] && pkgs+=("$PLATFORM")

echo "Stowing: ${pkgs[*]} -> $HOME"
stow --dir="$REPO" --target="$HOME" --restow --verbose "${pkgs[@]}"

# ----- tools -----
# Cross-platform CLI tools are declared in shared/.config/mise/config.toml,
# which stow just linked to ~/.config/mise/config.toml. Install them.
export PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/local/bin:$PATH"
if command -v mise >/dev/null 2>&1; then
  echo "Installing tools with mise..."
  mise install -y
fi
