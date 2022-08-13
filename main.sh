#!/usr/bin/env bash

# DEBUG OPTIONS
set -euo pipefail

SKIP=false

print_usage() {
    printf "Usage: ..."
}

while getopts ':vb' flag; do
    case "${flag}" in
        b) SKIP=true ;;
        *)
            print_usage
            exit 1
            ;;
    esac
done

echo $SKIP

HERE="$(dirname "$0")"

# declare -r RB=(ruby s-tools-dots.rb)
# declare -r GO=(go s-tools-dots.go)

# for runner in ${RB[@]} ${GO[@]}; do
# echo ${runner[@]}
# [[ -x $(command -v ${runner[0]}) ]] && ruby "$HERE/${runner[1]}" "$@"

# done
