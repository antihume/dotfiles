# vterm integration
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    export TERM=xterm-256color
fi

# Completion
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select

# Plugins (if available)
[[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && \
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[[ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && \
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Git integration
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git:*' formats ' %F{cyan}(%b%u%c)%f'
zstyle ':vcs_info:git:*' actionformats ' %F{cyan}(%b|%a%u%c)%f'

precmd() { vcs_info }

# Simple prompt
PROMPT='%F{green}%n@%m%f %F{blue}%1~%f${vcs_info_msg_0_} %F{white}$%f '

# History
export HISTFILE=~/.zsh_history
export HISTSIZE=16384
export SAVEHIST=16384
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

# Editor
export EDITOR="helix"
export VISUAL="helix"

# Aliases
alias l='eza --icons --group-directories-first'
alias ll='eza -l --icons --group-directories-first --git'
alias la='eza -la --icons --group-directories-first --git'
alias tree='eza --tree --icons'
alias e='emacsclient -c -a ""'
alias et='emacsclient -t -a ""'
alias hx='helix'

# opam configuration
[[ ! -r '/home/antihume/.opam/opam-init/init.zsh' ]] || source '/home/antihume/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
