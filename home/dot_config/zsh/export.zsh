export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"

# sccache settings
export SCCACHE_CACHE_SIZE=100G
export RUSTC_WRAPPER="sccache"
export DIRENV_LOG_FORMAT=

export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep/ripgrep.conf
export GNUPGHOME=$XDG_CONFIG_HOME/gnupg
