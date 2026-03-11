# -*- shell-script -*-
# Aliases for listing files and directories with eza
alias ls='eza -al --color=always --group-directories-first'
alias la='eza -a --color=always --group-directories-first'
alias ll='eza -l --color=always --group-directories-first'
alias lt='eza -aT --color=always --group-directories-first'

# Starts the calculator with math support.
alias bc='bc -l'

# Makes file commands verbose for better visibility.
alias cp='cp -v'
alias mv='mv -v'

# Creates parent directories on demand.
alias mkdir='mkdir -pv'

# Displays disk space in a human-readable format.
alias df='df -h'

# Colorizes the diff output if possible.
if type 'colordiff' &>/dev/null; then
	alias diff='colordiff'
fi

# Prints disk usage per directory non-recursively, sorted in human-readable format.
alias du='du -hs * | sort -rh'

# Stops ping after sending 4 ECHO_REQUEST packets.
alias ping='ping -c 4'

# Enables simple aliases to be used with sudo.
alias sudo='sudo '

# Gets external IP address.
if command -v curl >/dev/null; then
	alias publicip='curl --silent --compressed --max-time 5 --url "https://ipinfo.io/ip"'
else
	alias publicip='wget -qO- --compression=auto --timeout=5 "https://ipinfo.io/ip"'
fi

if [ -z "$CLAUDE_SESSION_ID" ]; then
	alias grep='rg --color=auto'
fi

# Fuzzy finder for tldr and man pages.
alias ftldr='print -rl -- ${(k)commands} | fzf | xargs tldr'
alias fman='print -rl -- ${(k)commands} | fzf | xargs man'

# Aliases for text editors.
alias cat='bat'
alias emacs="emacsclient -c -a 'emacs'"
alias doom="emacsclient -c -s doom -a 'emacs'"
alias em="emacsclient -t -a 'emacs -ntw --with-profile doom'"
alias dem="emacsclient -t -s doom -a 'emacs -ntw --with-profile doom'"
alias vim="nvim"
alias vi="\vim"

# Chezmoi configuration editing.
alias cz='chezmoi'
alias viz='chezmoi edit .zshrc'
alias vit='chezmoi edit ~/.config/tmux/tmux.conf'
alias vic='chezmoi edit'
alias src='source ~/.zshrc'
alias tsrc='tmux source ~/.config/tmux/tmux.conf'

# Tmux session management.
alias t='tmux attach || tmux new-session'
alias ta='tmux attach -t'
alias tn='tmux new-session'
alias tls='tmux list-sessions'

# Simplified directory navigation.
alias cd-='cd -'
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

# Exits the shell.
alias :q='exit'

# Clears the terminal.
alias c='clear'

# Quick navigation to specific directories.
alias dls='cd $HOME/Downloads'
alias docs='cd $HOME/Documents'
alias dt='cd $HOME/Desktop'
alias cdb='cd $HOME/code/booktoken-web'

# Aliases for common, but unused programs
alias neofetch='fastfetch'

# rebind 'gs' from ghostscript to 'git status'
alias gs="git status"

# Aliases under tmux.
if [ -n "$TMUX" ]; then
	unalias ssh 2>/dev/null
	alias td='tmux detach'
fi
