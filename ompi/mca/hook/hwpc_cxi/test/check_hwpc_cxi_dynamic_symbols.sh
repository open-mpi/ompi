#!/bin/sh
# -*- Mode: sh; c-basic-offset:4 ; indent-tabs-mode:nil -*-
#
# SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#
# Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# Check that component-private constants helpers do not leak from libopen_mpi.

set -eu

if [ "$#" -lt 3 ]; then
    echo "Usage: $0 <ompi-.libs-directory> <opal_config.h> <nm> [nm-arguments...]" >&2
    exit 2
fi

libdir="$1"
config_header="$2"
shift 2
# "$@" is now the nm command and its arguments (libtool may set NM to, e.g., "/usr/bin/nm -B").
library=""

if [ ! -f "$config_header" ]; then
    echo "ERROR: configuration header not found: $config_header" >&2
    exit 1
fi

# Without visibility support OMPI_HIDDEN expands to nothing, so private symbols are legitimately exported.
if ! grep '^#define OPAL_C_HAVE_VISIBILITY 1' "$config_header" >/dev/null 2>&1; then
    echo "SKIP: symbol visibility is unsupported or disabled (OPAL_C_HAVE_VISIBILITY is not 1)"
    exit 0
fi

for candidate in "$libdir/libopen_mpi.so" "$libdir/libopen_mpi.dylib"; do
    if [ -f "$candidate" ]; then
        library="$candidate"
        break
    fi
done

if [ -z "$library" ]; then
    echo "SKIP: no shared libopen_mpi found (static-only build)"
    exit 0
fi

if ! command -v "$1" >/dev/null 2>&1; then
    echo "ERROR: symbol tool not found: $1" >&2
    exit 1
fi

case "$(uname -s)" in
    Linux*)
        symbols=$("$@" -D --defined-only "$library")
        ;;
    Darwin*)
        symbols=$("$@" -gU "$library")
        ;;
    *)
        echo "SKIP: dynamic symbol audit is not configured for $(uname -s)"
        exit 0
        ;;
esac

leaked_symbols=$(printf '%s\n' "$symbols" | awk '
    NF {
        symbol = $NF
        sub(/^_/, "", symbol)
        sub(/@.*/, "", symbol)
        if (symbol ~ /^hwpc_cxi_/ ||
            symbol ~ /^mca_hook_hwpc_cxi_/ ||
            symbol ~ /^ompi_hook_hwpc_cxi_/) {
            print symbol
        }
    }
' | sort -u)

if [ -n "$leaked_symbols" ]; then
    echo "ERROR: HWPC-CXI private symbols exported from libopen_mpi:" >&2
    printf '%s\n' "$leaked_symbols" | sed 's/^/    /' >&2
    exit 1
fi

exit 0
