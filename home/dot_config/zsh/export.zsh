export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"

PERL5LIB="/home/phillip/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="/home/phillip/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
PERL_MB_OPT="--install_base \"/home/phillip/perl5\""
export PERL_MB_OPT
PERL_MM_OPT="INSTALL_BASE=/home/phillip/perl5"
export PERL_MM_OPT
# sccache settings
export SCCACHE_CACHE_SIZE=100G
export CMAKE_C_COMPILER_LAUNCHER=sccache
export CMAKE_CXX_COMPILER_LAUNCHER=sccache
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
export DIRENV_LOG_FORMAT=

export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep/ripgrep.conf
