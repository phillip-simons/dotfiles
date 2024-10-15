# -*- shell-script -*-

# Uses `fd` to search for directories (including hidden ones) and `fzf` to select one to navigate to
# - Allows specifying a maximum search depth via an argument (defaults to 1 if not provided)
# - Includes hidden directories, but excludes `.git` and `target` directories
# - Allows you to select a directory using `fzf` and navigates to the selected one
# Usage: fcd [max-depth]
function fcd() {
    local max_depth=${1:-1}  # Set max depth to the first argument, default to 1 if not provided
    local folder=$(fd --type d --max-depth "$max_depth" --hidden --exclude .git --exclude target | fzf)
    if [[ -n "$folder" ]]; then
        cd "$folder"
    fi
}

# Navigates to the specified directory and lists its contents
# Usage: cdls <dir>
function cdls() {
    local dir="$1"
    dir="${dir:=$HOME}"  # Default to HOME if no argument is provided
    if [[ -d "$dir" ]]; then
        cd "$dir" >/dev/null
        command ls  # Use your `ls` alias if it's defined
    else
        echo "cdls: $dir: Directory not found"
    fi
}

# Extracts common file formats.
# Syntax: `extract <file1>`
# Source: https://github.com/xvoland/Extract
# TODO extract in named folder
function extract {
    if [ -z "$1" ]; then
        echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
        echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
        return 0
    fi

    for n in "$@"
    do
    if [ -f "$n" ] ; then
        case "${n%,}" in
            *.cbt|*.tar.bz2|*.tar.gz|*.tar.xz|*.tbz2|*.tgz|*.txz|*.tar)
                         tar xvf "$n"       ;;
            *.lzma)      unlzma ./"$n"      ;;
            *.bz2)       bunzip2 ./"$n"     ;;
            *.cbr|*.rar)       unrar x -ad ./"$n" ;;
            *.gz)        gunzip ./"$n"      ;;
            *.cbz|*.epub|*.zip)       unzip ./"$n"       ;;
            *.z)         uncompress ./"$n"  ;;
            *.7z|*.apk|*.arj|*.cab|*.cb7|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.pkg|*.rpm|*.udf|*.wim|*.xar)
                         7z x ./"$n"        ;;
            *.xz)        unxz ./"$n"        ;;
            *.exe)       cabextract ./"$n"  ;;
            *.cpio)      cpio -id < ./"$n"  ;;
            *.cba|*.ace)      unace x ./"$n"      ;;
            *.zpaq)      zpaq x ./"$n"      ;;
            *.arc)         arc e ./"$n"       ;;
            *.cso)       ciso 0 ./"$n" ./"$n.iso" && \
                              extract "$n".iso && \rm -f "$n" ;;
            *)
                         echo "extract: '$n' - unknown archive method"
                         return 1
                         ;;
        esac
    else
        echo "'$n' - file does not exist"
        return 1
    fi
    done
}
alias x=extract

# Searches history for a string, or lists all history.
# Syntax: `historysearch <string>`
function history_search() {
    if [ -z "$1" ]; then
        history
    else
        history | grep "$1"
    fi
}

# Searches session history for a string, or lists all session history.
# Syntax: `history_session_search <string>`
function history_session_search() {
    prefix=$(date +"$HISTTIMEFORMAT")
    offset=$((8 + ${#prefix}))
    comm -23 <(history | cut -c ${offset}-) "${HISTFILE:-'~/.bash_history'}" | grep "$1"
}

# Creates a directory and changes to it.
# Syntax: `mkcd <directory>`
function mkcd() {
    if [ -z "$1" ]; then
        echo "Usage: mkcd <path>"
        echo "Help: mkcd creates a directory if it doesn't exist, then changes to it."
        return 0
    fi

    mkdir -p -- "$@" && builtin cd -P -- "$_" || {
        echo "Failed to create or change directory."
        return 1
    };
}

# Repeats a command a set number of times.
# Syntax: `repeat <count> <command>`
function repeat() {
    if [ -z "$1" ] || [ "$#" -lt 2 ]; then
        echo "Usage: repeat <count> <command> ..."
        echo "Help: repeat runs a command x number of times."
        return $#
    fi

    local i max
    max=$1; shift;
    for ((i=1; i <= max ; i++)); do
        eval "$@";
    done
}
alias r=repeat
