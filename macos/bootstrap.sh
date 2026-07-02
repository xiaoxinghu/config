#!/usr/bin/env bash
# Bootstrap Homebrew on macOS and install everything from the Brewfile.
# Installs Homebrew if missing, loads it into the environment, then runs
# `brew bundle` against the Brewfile next to this script. Idempotent.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Homebrew's default prefix depends on the CPU architecture.
case "$(uname -m)" in
  arm64) PREFIX="/opt/homebrew" ;;
  *)     PREFIX="/usr/local" ;;
esac

# Install Homebrew if it isn't already available.
if ! command -v brew >/dev/null 2>&1 && [[ ! -x "$PREFIX/bin/brew" ]]; then
  echo "Installing Homebrew..."
  NONINTERACTIVE=1 /bin/bash -c \
    "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Load Homebrew into the current shell (PATH, MANPATH, etc.).
eval "$("$PREFIX/bin/brew" shellenv)"

# Install/upgrade everything declared in the Brewfile.
echo "Installing packages from Brewfile..."
brew bundle install --file="$HERE/Brewfile"
