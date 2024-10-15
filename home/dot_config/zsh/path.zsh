. "$HOME/.cargo/env"
export PATH="$PATH:$HOME/.ghcup/bin"
export PATH="$PATH:$HOME/.nix-profile/bin"
export PATH="$PATH:$HOME/.ghcup/bin"

export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$PATH:/home/phillip/.local/bin"

export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/perl5/bin:$PATH"

typeset -U path
PATH=($^PATH(N-/))

export PATH
