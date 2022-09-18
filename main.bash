#!/usr/bin/env bash

# Debug Options
set -euo pipefail

# DESCRIPTION: Yet another simple and opinionated dot files manager.

# VARIABLES
ARGS=() # make args an array, not a string
TO=""
FROM=""
CREATE=false
REMOVE=false
PRETEND=false
OVERWRITE=false

# CLI OPTIONS
print_usage() {
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

print_info() {
    cat <<EOF
dot [information]
to:         $TO
from:       $FROM
create:     $CREATE
remove:     $REMOVE
pretend:    $PRETEND
overwrite:  $OVERWRITE
EOF
}

exit_abnormal() { # Function: Exit with error.
    print_usage
    exit 1
}

# Replace long arguments
for arg; do
    case "$arg" in
        --to) ARGS+=(-t) ;;
        --from) ARGS+=(-f) ;;
        --create) ARGS+=(-c) ;;
        --remove) ARGS+=(-r) ;;
        --pretend) ARGS+=(-p) ;;
        --overwrite) ARGS+=(-o) ;;
        --help) ARGS+=(-h) ;;
        *) ARGS+=("$arg") ;;
    esac
done

set -- "${ARGS[@]}"

while getopts "crpoiht:f:" OPTION; do
    : "$OPTION" "$OPTARG"
    case $OPTION in
        t) TO="$OPTARG" ;;
        f) FROM="$OPTARG" ;;
        c) CREATE=true ;;
        r) REMOVE=true ;;
        p) PRETEND=true ;;
        o) OVERWRITE=true ;;
        h) print_usage ;;
        i) print_info ;;
        *) exit_abnormal ;;
    esac
done

exit
