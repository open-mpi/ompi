#!/bin/bash
# -*- Mode: sh; c-basic-offset:4 ; indent-tabs-mode:nil -*-
#
# SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
# SPDX-License-Identifier:  MIT
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Build and run HWPC_CXI tests using rapid_rebuild.sh environment
# This script demonstrates the recommended way to build tests with proper
# library paths and dependencies from the rapid_rebuild.sh system.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# ============================================================================
# STEP 1: Source or extract environment from rapid_rebuild.sh
# ============================================================================

# These are the library paths from rapid_rebuild.sh (lines 11-20)
# You can override these with environment variables if needed

export LIBFABRIC_PREFIX="${LIBFABRIC_PREFIX:-__REPLACE_ME_LIBFABRIC_PREFIX__}"
export JSONC_LIBDIR="${JSONC_LIBDIR:-__REPLACE_ME_JSONC_LIBDIR__}"
export JSONC_LINK_SO="${JSONC_LINK_SO:-$JSONC_LIBDIR/libjson-c.so.5.0.0}"

if [[ "$LIBFABRIC_PREFIX" == __REPLACE_ME_* || "$JSONC_LIBDIR" == __REPLACE_ME_* ]]; then
    echo "ERROR: placeholder paths detected. Set LIBFABRIC_PREFIX and JSONC_LIBDIR to real install locations." >&2
    echo "  LIBFABRIC_PREFIX=$LIBFABRIC_PREFIX" >&2
    echo "  JSONC_LIBDIR=$JSONC_LIBDIR" >&2
    exit 1
fi

# CRITICAL: This LD_LIBRARY_PATH allows libtool and the runtime to find all dependencies
# This must match what rapid_rebuild.sh uses (line 369)
export LD_LIBRARY_PATH="$LIBFABRIC_PREFIX/lib:$JSONC_LIBDIR:${LD_LIBRARY_PATH:-}"

# These flags are used by rapid_rebuild.sh for OMPI (lines 265-267)
export CPPFLAGS="-I$LIBFABRIC_PREFIX/include/rdma"
export LDFLAGS="-L$LIBFABRIC_PREFIX/lib -L$JSONC_LIBDIR -Wl,-rpath,$LIBFABRIC_PREFIX/lib:$JSONC_LIBDIR"

# ============================================================================
# STEP 2: Build the tests
# ============================================================================

echo "Building HWPC_CXI tests with environment from rapid_rebuild.sh:"
echo ""
echo "Environment:"
echo "  LIBFABRIC_PREFIX:  $LIBFABRIC_PREFIX"
echo "  JSONC_LIBDIR:      $JSONC_LIBDIR"
echo "  JSONC_LINK_SO:     $JSONC_LINK_SO"
echo "  LD_LIBRARY_PATH:   $LD_LIBRARY_PATH"
echo "  CPPFLAGS:          $CPPFLAGS"
echo "  LDFLAGS:           $LDFLAGS"
echo ""
echo "Building tests..."

cd "$SCRIPT_DIR"

# Pass library paths to make as environment variables
make clean
LIBFABRIC_PREFIX="$LIBFABRIC_PREFIX" \
JSONC_LIBDIR="$JSONC_LIBDIR" \
JSONC_LINK_SO="$JSONC_LINK_SO" \
make all

if [[ $? -ne 0 ]]; then
    echo "ERROR: Build failed!"
    exit 1
fi

echo ""
echo "SUCCESS! Supported HWPC_CXI validation binary built successfully:"
ls -lh hwpc_cxi_sendrecv_test | awk '{print "  " $9 " (" $5 ")"}'
echo ""
echo "To run the supported validation flow:"
echo "  ./run_hwpc_cxi_validate.sh 4 2 100"
echo ""
echo "To link against the built libraries at runtime, ensure these are in LD_LIBRARY_PATH:"
echo "  export LD_LIBRARY_PATH=$LIBFABRIC_PREFIX/lib:$JSONC_LIBDIR:\$LD_LIBRARY_PATH"
