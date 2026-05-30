#!/bin/bash
eval "$(/opt/homebrew/bin/brew shellenv)"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
# Respect .nvmrc if present in the current directory
[ -f ".nvmrc" ] && nvm use --silent 2>/dev/null
