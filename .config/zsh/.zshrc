# Disable Powerlevel10k instant prompt cache (it interferes with compinit)
export POWERLEVEL9K_INSTANT_PROMPT=off
unsetopt prompt_cr prompt_subst prompt_sp

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

autoload -Uz compinit
compinit -C
zmodload zsh/complist
zstyle ':completion:*' menu select

# (Optional) Jump straight into the menu on first Tab
bindkey '^I' menu-select   # ^I is Tab

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

# ============================================================================
# ALIASES
# ============================================================================

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias lazyconfig='lazygit --git-dir=$HOME/.cfg/ --work-tree=$HOME'

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

if type brew &>/dev/null; then
		FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

# ============================================================================
# 4. Initialize completions FIRST
# ============================================================================

# 2) Arrow keys will now navigate; add vim-style hjkl too:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char

# (Optional) Accept with Ctrl-y (in menu) and execute immediately with Enter
bindkey -M menuselect '^Y' accept-search
bindkey -M menuselect '^M' .accept-line

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh


GITSTATUS_LOG_LEVEL=DEBUG

eval "$(zoxide init zsh)"
eval "$(direnv hook zsh)"

# --- plugins ---
source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $(brew --prefix)/share/zsh-history-substring-search/zsh-history-substring-search.zsh

# --- key binds ---
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# load bun completions
if (( $+commands[bun] )); then
  [ -s ~/.bun/_bun ] || bun completions
	fpath+=~/.bun/
fi

# autocompletion via carapace
export CARAPACE_BRIDGES='zsh,fish,bash,inshellisense' # optional
zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
source <(carapace _carapace)
