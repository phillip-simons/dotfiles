# -*- shell-script -*-
# Aliases for listing files and directories with lsd
alias ls='lsd -al --color=always --group-directories-first'             # my preferred listing
alias la='lsd -a --color=always --group-directories-first'              # list all files and directories
alias ll='lsd -l --color=always --group-directories-first'              # long format listing
alias lt='lsd -aT --color=always --group-directories-first'             # tree listing
alias l.='lsd -al --color=always --group-directories-first ../'         # list parent directory
alias l..='lsd -al --color=always --group-directories-first ../../'     # list 2 levels up
alias l...='lsd -al --color=always --group-directories-first ../../../' # list 3 levels up

# Starts the calculator with math support.
alias bc='bc -l'

# Makes file commands verbose for better visibility.
alias cp='cp -v' # verbose copy
alias mv='mv -v' # verbose move

# Creates parent directories on demand.
alias mkdir='mkdir -pv' # create parent directories, verbose

# Displays disk space in a human-readable format.
alias df='df -h'

# Colorizes the diff output if possible.
if type 'colordiff' &>/dev/null; then
	alias diff='colordiff' # use colordiff if available
fi

# Prints disk usage per directory non-recursively, sorted in human-readable format.
alias du='du -hs * | sort -rh'

# Stops ping after sending 4 ECHO_REQUEST packets.
alias ping='ping -c 4'

# Enables simple aliases to be used with sudo.
alias sudo='sudo ' # allows sudo to be applied to aliases

# Searches command history.
alias h='history_search'          # custom function for searching history
alias hs='history_session_search' # custom function for session history search

# Gets local IP address.
alias localip="ip route get 1 | awk '{print \$NF;exit}'"

# Gets external IP address using different methods based on available commands.
if command -v dig &>/dev/null; then
	alias publicip='dig +short myip.opendns.com @resolver1.opendns.com' # use dig if available
elif command -v curl >/dev/null; then
	alias publicip='curl --silent --compressed --max-time 5 --url "https://ipinfo.io/ip"' # use curl if available
else
	alias publicip='wget -qO- --compression=auto --timeout=5 "https://ipinfo.io/ip"' # fallback to wget
fi

# Calculates MD5 hashes using available commands.
if ! command -v md5sum >/dev/null; then
	if command -v md5 >/dev/null; then
		alias md5sum='md5 -r' # use md5 if available
	else
		alias md5sum='openssl md5 -r' # fallback to openssl
	fi
fi

# Calculates SHA1 hashes.
if ! command -v sha1sum >/dev/null; then
	if command -v shasum >/dev/null; then
		alias sha1sum='shasum -a 1 -p' # use shasum if available
	else
		alias sha1sum='openssl sha1 -r' # fallback to openssl
	fi
fi

# Calculates SHA256 hashes.
if ! command -v sha256sum >/dev/null; then
	if command -v shasum >/dev/null; then
		alias sha256sum='shasum -a 256 -p' # use shasum if available
	else
		alias sha256sum='openssl sha256 -r' # fallback to openssl
	fi
fi

# Displays detailed weather and forecast.
if command -v curl >/dev/null; then
	alias forecast='curl --silent --compressed --max-time 10 --url "https://wttr.in?F"' # use curl for forecast
else
	alias forecast='wget -qO- --compression=auto --timeout=10 "https://wttr.in?F"' # fallback to wget
fi

# Displays current weather.
if command -v curl >/dev/null; then
	alias weather='curl --silent --compressed --max-time 10 --url "https://wttr.in/?format=%l:+(%C)+%c++%t+\[%h,+%w\]"' # use curl for weather
else
	alias weather='wget -qO- --compression=auto --timeout=10 "https://wttr.in/?format=%l:+(%C)+%c++%t+\[%h,+%w\]"' # fallback to wget
fi

alias grep='rg --color=auto' # colorize grep output (good for log files)

# Fuzzy finder for tldr and man pages.
alias ftldr='compgen -c | fzf | xargs tldr' # tldr page finder
alias fman='compgen -c | fzf | xargs man'   # man page finder

# Aliases for text editors.
alias cat='bat'                                                        # use bat instead of cat for syntax highlighting
alias emacs="emacsclient -c -a 'emacs'"                                # GUI version of Spacemacs
alias doom="emacsclient -c -s doom -a 'emacs'"                         # GUI version of Doom Emacs
alias em="emacsclient -t -a 'emacs -ntw --with-profile doom'"          # Terminal version of Spacemacs
alias dem="emacsclient -t -s doom -a 'emacs -ntw --with-profile doom'" # Terminal version of Doom Emacs
alias vim="nvim"                                                       # use nvim instead of vim
alias vi="\vim"                                                        # alias for vi

# Chezmoi configuration editing.
alias cz='chezmoi'                                # alias for chezmoi
alias viz='chezmoi edit .zshrc'                   # edit .zshrc with chezmoi
alias vit='chezmoi edit ~/.config/tmux/tmux.conf' # edit tmux config
alias vic='chezmoi edit'                          # edit chezmoi configuration
alias src='source ~/.zshrc'                       # source zsh configuration
alias tsrc='tmux source ~/.config/tmux/tmux.conf' # source tmux configuration

# Tmux session management.
alias t='tmux attach || tmux new-session' # attach to tmux session or create new
alias ta='tmux attach -t'                 # attach to specified tmux session
alias tn='tmux new-session'               # create new tmux session
alias tls='tmux list-sessions'            # list tmux sessions

# Simplified directory navigation.
alias cd-='cd -'             #Return to last directory
alias cd..='cd ..'           # move up one directory
alias ..='cd ..'             # same as above
alias ...='cd ../..'         # move up two directories
alias cd ...='cd ../..'      # same as above with explicit command
alias .3='cd ../../..'       # move up three directories
alias .4='cd ../../../..'    # move up four directories
alias .5='cd ../../../../..' # move up five directories

# Exits the shell.
alias :q='exit' # exit command

# Clears the terminal.
alias c='clear' # clear the screen

# Quick navigation to specific directories.
alias dls='cd $HOME/Downloads'  # navigate to Downloads
alias docs='cd $HOME/Documents' # navigate to Documents
alias dt='cd $HOME/Desktop'     # navigate to Desktop

# Aliases for common, but unused programs
alias neofetch='fastfetch'

# Restart Emacs server if not running.
alias rem="killall emacs || echo 'Emacs server not running'; /usr/bin/emacs --daemon" # restart Emacs server

# Aliases under tmux for different contexts.
if [ -n "$TMUX" ]; then
	# Inside tmux
	unalias ssh 2>/dev/null # remove existing ssh alias
	alias td='tmux detach'  # detach from tmux session
else
	# Outside tmux
	alias ssh='kitten ssh' # use kitten for ssh
fi
