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
