# Created by newuser for 5.9


# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!
export PATH="/home/rijan/.juliaup/bin:$PATH"
export PATH="/home/rijan/.cargo/bin:$PATH"
export PATH="/home/rijan/executables:$PATH"
export PATH="/home/rijan/.nimble/bin:$PATH"
export PATH="~/.bun/bin:$PATH"
# <<< juliaup initialize <<<

export wip="/home/rijan/work_in_progress/"
# sourcing plugins

source /home/rijan/community_repos/zsh-autosuggestions.zsh

# Find and set branch name var if in git repository.
function git_branch_name()
{
  branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
  if [[ $branch == "" ]];
  then
    :
  else
    echo '- ('$branch')'
  fi
}

# Enable substitution in the prompt.
setopt prompt_subst

# Config for prompt. PS1 synonym.
prompt='%2/ $(git_branch_name) > '

export EDITOR=hx


function gitup() {
	git add .
	git commit -a -m "$1"
	git push
}

uahvpn(){
	sudo openconnect --protocol=nc -C "DSID="$1 psvpn.uah.edu
}

function newterim() {
  mkdir -p "$1/blackbox"
  mkdir -p "$1/input"
  mkdir -p "$1/output"
  touch "$1/README.md"
}

compress_directory() {
    # Default compression level
    local COMP_LEVEL=6

    # Parse options
    while getopts ":l:" opt; do
        case ${opt} in
            l)
                COMP_LEVEL=$OPTARG
                ;;
            \?)
                echo "Invalid option: -$OPTARG" >&2
                return 1
                ;;
            :)
                echo "Option -$OPTARG requires an argument." >&2
                return 1
                ;;
        esac
    done
    shift $((OPTIND -1))

    # Check if an argument is provided
    if [ $# -ne 1 ]; then
        echo "Usage: compress_directory [-l level] <directory>"
        return 1
    fi

    # Get the directory name from the argument
    local SOURCE_DIR="$1"

    # Extract the basename of the directory
    local BASENAME=$(basename "$SOURCE_DIR")

    # Create a tar archive of the directory
    tar -cvf - "$SOURCE_DIR" | zstd --ultra -$COMP_LEVEL -o "$BASENAME.tar.zst"

    echo "Compressed archive saved as $BASENAME.tar.zst"
}

dandrive_copy(){
    rclone copy "$1" dandrive:work_in_progress/"$1"
}

dandrive_sync(){
    
    rclone sync "$1" dandrive:work_in_progress/"$1"
}

export FZF_DEFAULT_OPTS="
    --bind 'ctrl-y:execute(readlink -f {} | xclip -selection clipboard)'
    --bind 'ctrl-alt-y:execute-silent(xclip -selection clipboard {})'
"

export JULIA_NUM_THREADS=16

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/rijan/miniforge3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/rijan/miniforge3/etc/profile.d/conda.sh" ]; then
        . "/home/rijan/miniforge3/etc/profile.d/conda.sh"
    else
        export PATH="/home/rijan/miniforge3/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/rijan/miniforge3/etc/profile.d/mamba.sh" ]; then
    . "/home/rijan/miniforge3/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<


eval "$(atuin init zsh)"

# bun completions
[ -s "/home/rijan/.bun/_bun" ] && source "/home/rijan/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba init' !!
export MAMBA_EXE="/nix/store/08hnfw2zhkjxa6nrhd7w7nv9hvn7iby2-micromamba-1.4.4/bin/micromamba";
export MAMBA_ROOT_PREFIX="/home/rijan/micromamba";
__mamba_setup="$("$MAMBA_EXE" shell hook --shell zsh --prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    if [ -f "/home/rijan/micromamba/etc/profile.d/micromamba.sh" ]; then
        . "/home/rijan/micromamba/etc/profile.d/micromamba.sh"
    else
        export  PATH="/home/rijan/micromamba/bin:$PATH"  # extra space after export prevents interference from conda init
    fi
fi
unset __mamba_setup
# <<< mamba initialize <<<
