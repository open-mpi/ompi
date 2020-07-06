#!/bin/bash
# Copyright (c) 2020 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# 
# Trivial helper script to git clean a tree and all of its submodules.

set -euo pipefail

# Top git dir
root=$(git rev-parse --show-toplevel)
cd $root

# Clean the top-level dir
echo "=== Cleaning top-level git directory"
git clean -dfx .

submodule_dirs=$(git submodule status | awk '{print $2}')
if test -z "$submodule_dirs"; then
    echo "No submodules to clean"
    exit 0
fi

for dir in $submodule_dirs; do
    echo "=== Cleaning submodule: $dir"
    cd $dir
    git clean -dfx .
    cd $root
done
