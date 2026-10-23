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

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <nm-command> <ompi-.libs-directory>" >&2
    exit 2
fi

nm="$1"
libdir="$2"
library=""

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

if ! command -v "$nm" >/dev/null 2>&1; then
    echo "ERROR: symbol tool not found: $nm" >&2
    exit 1
fi

case "$(uname -s)" in
    Linux*)
        symbols=$("$nm" -D --defined-only "$library")
        ;;
    Darwin*)
        symbols=$("$nm" -gU "$library")
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
            (symbol ~ /^ompi_hook_hwpc_cxi_/ &&
             symbol !~ /^ompi_hook_hwpc_cxi_(init|fini)$/)) {
            print symbol
        }
    }
' | sort -u)

if [ -n "$leaked_symbols" ]; then
    echo "ERROR: HWPC-CXI private symbols exported from libopen_mpi:" >&2
    printf '%s\n' "$leaked_symbols" | sed 's/^/    /' >&2
    exit 1
fi

exit 0#!/bin/sh
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

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <nm-command> <ompi-.libs-directory>" >&2
    exit 2
fi

nm="$1"
libdir="$2"
library=""

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

if ! command -v "$nm" >/dev/null 2>&1; then
    echo "ERROR: symbol tool not found: $nm" >&2
    exit 1
fi

case "$(uname -s)" in
    Linux*)
        symbols=$("$nm" -D --defined-only "$library")
        ;;
    Darwin*)
        symbols=$("$nm" -gU "$library")
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
        if (symbol ~ /^hwpc_cxi_/) {
            print symbol
        } else if (symbol ~ /^ompi_hook_hwpc_cxi_/ &&
                   symbol !~ /^ompi_hook_hwpc_cxi_(init|fini|mpi_init_bottom|mpi_finalize_top)$/) {
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