# -*- mode: shell-script -*-
# Login shell configuration

export BUN_INSTALL="$HOME/.bun"
export PATH="$HOME/.local/bin:$BUN_INSTALL/bin:$PATH"

# Drop straight into tmux on SSH login. Runs here (login shell, before .zshrc)
# so tmux takes over ahead of p10k's instant prompt. Guards: interactive shell
# only (never scp/rsync/`ssh host cmd`), only over SSH (harmless on local
# terminals, incl. macOS), and not already inside tmux (no nesting). `-A`
# attaches to `main` if it exists, else creates it. On clean detach we `exit`
# so the SSH connection closes; if tmux can't start we fall through to a normal
# shell instead of locking you out.
if [[ -o interactive && -n "$SSH_CONNECTION" && -z "$TMUX" ]] && command -v tmux &>/dev/null; then
  tmux new-session -A -s main && exit
fi
