#!/usr/bin/env bash

# Debug Options
set -euo pipefail

# DESCRIPTION: Yet another simple and opinionated dot files manager.
# DEPENDENCIES: getopt,

# VARIABLES
TO=""
FROM=""
CREATE=false
REMOVE=false
PRETEND=false
OVERWRITE=false
IGNORED=()
INFO=false

# CLI OPTIONS
usage() {
    cat <<EOF
Usage: dot [options]
    --to TO                 destination folder to deliver links
    --from FROM             target folder with dotfiles
    --create                create links of dotfiles
    --remove                remove links from target folder
    --pretend               demonstrate files linking
    --overwrite             overwrite existent links
EOF
    exit 0
}

info() {
    cat <<EOF
 -- general information --
to:         $TO
from:       $FROM
create:     $CREATE
remove:     $REMOVE
pretend:    $PRETEND
overwrite:  $OVERWRITE
ignored:    ${IGNORED[@]}
EOF
    exit 0
}

SHORT=c,r,p,o,i,h,t:,f:
LONG=create,remove,pretend,overwrite,help,info,to:,from:
eval set -- "$(getopt -n dot --options $SHORT --longoptions $LONG -- "$@")"

# No arguments provided
[[ $# -eq 0 ]] && usage

while true; do # keep on till there is no more arguments
    case "$1" in
        -t | --to)
            _to="$2"
            TO=${_to%%/} # remove trailing slash
            shift 2
            ;;
        -f | --from)
            _from="$2"
            FROM=${_from%%/} # remove trailing slash
            shift 2
            ;;
        -h | --help) usage ;;
        -c | --create)
            CREATE=true
            break
            ;;
        -r | --remove)
            REMOVE=true
            break
            ;;
        -p | --pretend)
            PRETEND=true
            break
            ;;
        -o | --overwrite)
            OVERWRITE=true
            break
            ;;
        -i | --info) INFO=true ;;
        --) exit 1 ;;
    esac
done

# LIST OF FILES TO BE IGNORED
IGNORED_FILE="$FROM/.dotsignore"
[[ -f $IGNORED_FILE ]] && readarray -t IGNORED <"$IGNORED_FILE"
IGNORED+=(.git) # user should set it, but lets be safe!


exit
