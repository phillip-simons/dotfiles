fzf_compgen_path() {
	fd --exclude ".git" --follow --hidden . "$1"
}

fzf_compgen_dir() {
	fd --exclude ".git" --follow --hidden --type d . "$1"
}

export FZF_ALT_C_COMMAND="fd --type d --exclude .git --exclude target --follow --hidden"
export FZF_DEFAULT_COMMAND="fd --type f --exclude .git --follow --hidden"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
