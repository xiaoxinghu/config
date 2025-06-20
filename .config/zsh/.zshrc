# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Powerlevel10k theme
source $(brew --prefix)/share/powerlevel10k/powerlevel10k.zsh-theme

# Lines configured by zsh-newuser-install
HISTFILE=~/.config/zsh/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/xiaoxing/.config/zsh/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

# ============================================================================
# ALIASES
# ============================================================================

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# --- ls ---
alias ls="eza"
alias l="eza -lbF --git"         # list, size, type, git
alias ll="eza -lbGF --git"       # long list
alias llm="eza -lbGd --git --sort=modified"  # long list, modified date sort
alias la="eza -lbhHigmSa --time-style=long-iso --git --color-scale"  # all list
alias lx="eza -lbhHigUmuSa@ --time-style=long-iso --git --color-scale"  # all + extended list
alias lS="eza -1"                # one column, just names
alias lt="eza --tree --level=2"  # tree

# --- vim ---
alias v="nvim"
alias vv="nvim \$(fzf)"

# --- other tools ---
alias ta="tmux a"
alias g="lazygit"
alias e="emacsclient"
alias love="/Applications/LOVE.app/Contents/MacOS/love"
alias man="batman"

GITSTATUS_LOG_LEVEL=DEBUG

eval "$(zoxide init zsh)"
eval "$(direnv hook zsh)"
