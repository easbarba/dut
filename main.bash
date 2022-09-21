#!/usr/bin/env bash

# Debug Options
set -euo pipefail

# DESCRIPTION: Yet another simple and opinionated dot files manager.
# DEPENDENCIES: bash:4, getopt, find, cat, echo, grep, wc

# VARIABLES
ARGS=()
IGNORED=()

declare -A VALUES
VALUES=([from]='' [to]='')

declare -A ACTIONS
ACTIONS=([create]=false [remove]=false [pretend]=false [overwrite]=false [info]=false)

# CLI OPTIONS
usage() {
    cat <<EOF
Usage: dot [options]
    -t DIR,  --to DIR                destination folder to deliver links.
    -f DIR,  --from DIR              target folder with dotfiles.
    -c,      --create                create links of dotfiles.
    -r,      --remove                remove links from target folder.
    -p,      --pretend               demonstrate files linking.
    -o,      --overwrite             overwrite existent links.
EOF
    exit 0
}

# No arguments provided
[[ $# -eq 0 ]] && usage

# -- OPTIONS:

# Replace long arguments
for arg; do
    case "$arg" in
        --help)           ARGS+=(-h) ;;
        --from)           ARGS+=(-f) ;;
        --to)             ARGS+=(-t) ;;
        --create)         ARGS+=(-c) ;;
        --remove)         ARGS+=(-r) ;;
        --pretend)        ARGS+=(-p) ;;
        --overwrite)      ARGS+=(-o) ;;
        --info)           ARGS+=(-i) ;;
        *)                ARGS+=("$arg") ;;
    esac
done

set -- "${ARGS[@]}"

while getopts "hcrpoif:t:" OPTION; do
    case $OPTION in
        h)  usage;;
        f)  VALUES[from]="$OPTARG";;
        t)  VALUES[to]="$OPTARG";;
        c)  ACTIONS[create]=true;;
        r)  ACTIONS[remove]=true;;
        p)  ACTIONS[pretend]=true;;
        o)  ACTIONS[overwrite]=true;;
        i)  ACTIONS[info]=true;;
        *) usage;;
    esac
done

# -- BUSSINESS LOGIC:

# --from is a must!
if [[ -z ${VALUES[from]} ]]; then
    echo "Missing required option: '--from DIR'"
    exit
fi

# LIST OF FILES TO BE IGNORED
IGNORED_FILE="${VALUES[from]}/.dotsignore"
[[ -f $IGNORED_FILE ]] && readarray -t IGNORED <"$IGNORED_FILE"
IGNORED+=(.git) # user should set it, but lets be safe!

print_info() {
    echo " -- Values"
    echo
    for v in "${!VALUES[@]}"; do
        echo "$v: ${VALUES[$v]}"
    done

    echo
    echo " -- Actions"
    echo

    for v in "${!ACTIONS[@]}"; do
        echo "$v: ${ACTIONS[$v]}"
    done

    echo
echo " -- Internals"
echo
    echo "ignored: ${IGNORED[@]}"

    exit 1
}

# ACTIONS
[[ ${ACTIONS[info]} ]] && print_info

exit

# ON-GOING
for f in $(find "$FROM"); do
    FILE="${f#${FROM}/}"

    IGNORED_FOUND=$(echo "${IGNORED[@]}" | grep -o "$FILE" | wc -w)
    [[ ! $IGNORED_FOUND -gt 0 ]] && continue # echo "ignoring: $FILE"
    echo "$FILE"
    # [[ $FILE =~ ^.git/ ]] && continue
done

exit
