# --- History -----------------------------------------------------------------
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY
setopt HIST_REDUCE_BLANKS

# --- Completion --------------------------------------------------------------
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' special-dirs true

# --- Key Bindings ------------------------------------------------------------
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward

# --- Git Integration ---------------------------------------------------------
autoload -Uz vcs_info
setopt PROMPT_SUBST
zstyle ':vcs_info:git:*' formats ' %F{white}(%b)%f'
precmd() { vcs_info }

# --- Prompt ------------------------------------------------------------------
PROMPT='%F{white}[%f%F{magenta}%n@%m%f %F{cyan}%~%f%F{white}]%f${vcs_info_msg_0_} %F{white}$%f '

# --- Environment -------------------------------------------------------------
export EDITOR="vim"
export VISUAL="vim"

if [[ -x /usr/bin/dircolors ]]; then
    eval "$(dircolors -b)"
fi

# --- Aliases -----------------------------------------------------------------
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias diff='diff --color=auto'

alias l='ls -CF'
alias ll='ls -lh'
alias la='ls -lAh'

alias ..='cd ..'
alias ...='cd ../..'
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -Iv'
alias mkdir='mkdir -pv'

# --- Plugins -----------------------------------------------------------------
[[ -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && \
    source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

[[ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && \
    source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# --- Misc Options ------------------------------------------------------------
setopt AUTO_CD
setopt CORRECT
setopt INTERACTIVE_COMMENTS
