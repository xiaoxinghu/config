# macOS-specific zsh config, sourced by shared/.config/zsh/.zshrc if present.

# Homebrew environment. The bin dir is already on PATH from .zshenv; this adds
# MANPATH, INFOPATH, sbin, the HOMEBREW_* vars, and the completions fpath
# (before compinit runs).
if command -v brew &>/dev/null; then
  eval "$(brew shellenv)"
  fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath)
fi

alias love="/Applications/LOVE.app/Contents/MacOS/love"
