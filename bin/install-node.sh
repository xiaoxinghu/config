#!/usr/bin/env bash
set -e

NVM_VERSION="v0.40.3"
NODE_VERSION="25"

if [ ! -d "$HOME/.nvm" ]; then
  curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_VERSION}/install.sh | bash
fi

\. "$HOME/.config/nvm/nvm.sh"

nvm install "$NODE_VERSION"
nvm alias default "$NODE_VERSION"

npm install -g corepack
corepack enable pnpm
