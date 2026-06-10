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

# Build HWPC_CXI tests using the proper environment from rapid_rebuild.sh
# This script sources the library paths and sets up LD_LIBRARY_PATH correctly

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Set default library paths from rapid_rebuild.sh configuration
JSONC_LIBDIR="${JSONC_LIBDIR:-/usr/lib64}"
LIBFABRIC_PREFIX="${LIBFABRIC_PREFIX:-__REPLACE_ME_LIBFABRIC_PREFIX__}"
OMPI_PREFIX="${OMPI_PREFIX:-__REPLACE_ME_OMPI_PREFIX__}"

if [[ "$LIBFABRIC_PREFIX" == __REPLACE_ME_* || "$OMPI_PREFIX" == __REPLACE_ME_* ]]; then
	echo "ERROR: placeholder paths detected. Set LIBFABRIC_PREFIX and OMPI_PREFIX to real install locations." >&2
	echo "  LIBFABRIC_PREFIX=$LIBFABRIC_PREFIX" >&2
	echo "  OMPI_PREFIX=$OMPI_PREFIX" >&2
	exit 1
fi

# Export the critical LD_LIBRARY_PATH to find all dependencies
export LD_LIBRARY_PATH="$LIBFABRIC_PREFIX/lib:$JSONC_LIBDIR:${LD_LIBRARY_PATH:-}"
export CPPFLAGS="-I$LIBFABRIC_PREFIX/include/rdma"
export LDFLAGS="-L$LIBFABRIC_PREFIX/lib -L$JSONC_LIBDIR -Wl,-rpath,$LIBFABRIC_PREFIX/lib:$JSONC_LIBDIR"
export LIBS="-lcxi"

echo "Building HWPC_CXI tests with environment:"
echo "  JSONC_LIBDIR:      $JSONC_LIBDIR"
echo "  LIBFABRIC_PREFIX:  $LIBFABRIC_PREFIX"
echo "  LD_LIBRARY_PATH:   $LD_LIBRARY_PATH"
echo "  CPPFLAGS:          $CPPFLAGS"
echo "  LDFLAGS:           $LDFLAGS"
echo "  LIBS:              $LIBS"
echo ""

# Build the tests
cd "$SCRIPT_DIR"
make clean
make all

echo "Build complete!"
