#!/usr/bin/env bash

# Debug Options
set -euo pipefail

# DESCRIPTION: Yet another simple and opinionated dot files manager.
# DEPENDENCIES: bash:4, getopt, find, cat, echo, grep, wc

# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

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
        --help) ARGS+=(-h) ;;
        --from) ARGS+=(-f) ;;
        --to) ARGS+=(-t) ;;
        --create) ARGS+=(-c) ;;
        --remove) ARGS+=(-r) ;;
        --pretend) ARGS+=(-p) ;;
        --overwrite) ARGS+=(-o) ;;
        --info) ARGS+=(-i) ;;
        *) ARGS+=("$arg") ;;
    esac
done

set -- "${ARGS[@]}"

while getopts "hcrpoif:t:" OPTION; do
    case $OPTION in
        h) usage ;;
        f) VALUES[from]="$OPTARG" ;;
        t) VALUES[to]="$OPTARG" ;;
        c) ACTIONS[create]=true ;;
        r) ACTIONS[remove]=true ;;
        p) ACTIONS[pretend]=true ;;
        o) ACTIONS[overwrite]=true ;;
        i) ACTIONS[info]=true ;;
        *) usage ;;
    esac
done

# -- BUSSINESS LOGIC:

# --from is a must!
[[ -z ${VALUES[from]} ]] && echo "Missing required option: '--from DIR'" && exit

# LIST OF FILES TO BE IGNORED
IGNORED_FILE="${VALUES[from]}/.dutignore"
[[ -f $IGNORED_FILE ]] && mapfile -t IGNORED <"$IGNORED_FILE"
IGNORED+=(.git) # user should set it, but lets be safe!

print_info() {
    echo "    -- Values
    "

    for v in "${!VALUES[@]}"; do
        echo "$v: ${VALUES[$v]}"
    done

    echo "
    -- Actions
    "

    for v in "${!ACTIONS[@]}"; do
        echo "$v: ${ACTIONS[$v]}"
    done

    echo "
    -- Internals
    "
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
