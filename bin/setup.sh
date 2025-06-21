#!/usr/bin/env bash

set -e

REPO_URL="git@github.com:xiaoxinghu/config.git"
BARE_DIR="$HOME/.cfg"

config() {
  /usr/bin/git --git-dir="$HOME/.cfg" --work-tree="$HOME" "$@"
}

echo "📦 Cloning bare repository into $BARE_DIR..."
git clone --bare "$REPO_URL" "$BARE_DIR"

echo "📁 Checking out dotfiles..."
config checkout 2>checkout_errors.log || {
    echo "⚠️  Some files would be overwritten:"
    grep -E "^\s+" checkout_errors.log | while read -r file; do
        echo "  Backing up $file to ${file}.backup"
        mv "$file" "${file}.backup"
    done
    config checkout
}
rm -f checkout_errors.log

echo "✅ Dotfiles checked out. The config alias and other settings are restored from the repo."
