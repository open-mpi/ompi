#!/bin/bash
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

# Combined helper that:
# 1) Sets the HWPC_CXI build/runtime environment
# 1.1) Allows
# 2) Builds hwpc_cxi_sendrecv_test
# 3) Runs run_hwpc_cxi_validate.sh (optional)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Environment defaults (override via shell env before invocation)
LIBFABRIC_PREFIX="${LIBFABRIC_PREFIX:-__REPLACE_ME_LIBFABRIC_PREFIX__}"
OMPI_PREFIX="${OMPI_PREFIX:-__REPLACE_ME_OMPI_PREFIX__}"

# Runtime defaults
NUM_PROCS=4
NUM_PPN=2
LOOPS=100
DO_BUILD=true
DO_RUN=true
SAVE_BASELINE=false

usage() {
    cat <<EOF
Usage: $(basename "$0") [options]

Options:
  --build-only         Build only; do not run validation
  --run-only           Run validation only; skip build
  --save-baseline      Pass --save-baseline to run_hwpc_cxi_validate.sh
  --num-procs N        Number of MPI ranks for validation (default: ${NUM_PROCS})
  --num-ppn N          Number of ranks per node (default: ${NUM_PPN})
  --loops N            Loop count for test binary (default: ${LOOPS})
  -h, --help           Show this help text

Environment overrides:
    LIBFABRIC_PREFIX     Path to a non-default libfabric install prefix
                                             (unset or placeholder uses the system default)
    OMPI_PREFIX          Path to Open MPI install prefix (optional if mpirun in PATH)
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --build-only)
            DO_BUILD=true
            DO_RUN=false
            ;;
        --run-only)
            DO_BUILD=false
            DO_RUN=true
            ;;
        --save-baseline)
            SAVE_BASELINE=true
            ;;
        --num-procs)
            NUM_PROCS="$2"
            shift
            ;;
        --num-ppn)
            NUM_PPN="$2"
            shift
            ;;
        --loops)
            LOOPS="$2"
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "ERROR: Unknown option: $1" >&2
            usage
            exit 1
            ;;
    esac
    shift
done

if [[ "$DO_BUILD" == "false" && "$DO_RUN" == "false" ]]; then
    echo "ERROR: Nothing to do (both build and run disabled)." >&2
    exit 1
fi

if [[ "$LIBFABRIC_PREFIX" == __REPLACE_ME_* ]]; then
    LIBFABRIC_PREFIX=""
fi

# Build and runtime linker search paths for the HWPC_CXI flow.
if [[ "$OMPI_PREFIX" == __REPLACE_ME_* ]]; then
    OMPI_PREFIX=""
else
    export PATH="$OMPI_PREFIX/bin:${PATH:-}"
    export LD_LIBRARY_PATH="$OMPI_PREFIX/lib:${LD_LIBRARY_PATH:-}"
fi

if [[ -n "$LIBFABRIC_PREFIX" ]]; then
    export LD_LIBRARY_PATH="$LIBFABRIC_PREFIX/lib:${LD_LIBRARY_PATH:-}"
    if [[ -n "${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH:-}" ]]; then
        export HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="$LIBFABRIC_PREFIX/lib:$HWPC_CXI_RUNTIME_LD_LIBRARY_PATH"
    else
        export HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="$LIBFABRIC_PREFIX/lib"
    fi
    LIBFABRIC_DISPLAY="$LIBFABRIC_PREFIX"
else
    LIBFABRIC_DISPLAY="<system default>"
    export HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH:-}"
fi

echo "HWPC_CXI combined build/run environment:"
echo "  LIBFABRIC_PREFIX:              $LIBFABRIC_DISPLAY"
echo "  OMPI_PREFIX:                   ${OMPI_PREFIX:-<PATH default>}"
echo "  LD_LIBRARY_PATH:               ${LD_LIBRARY_PATH:-<system default>}"
echo "  HWPC_CXI_RUNTIME_LD_LIBRARY_PATH: $HWPC_CXI_RUNTIME_LD_LIBRARY_PATH"
echo

cd "$SCRIPT_DIR"

if [[ "$DO_BUILD" == "true" ]]; then
    echo "Building HWPC_CXI test binary..."
    make clean
    LIBFABRIC_PREFIX="$LIBFABRIC_PREFIX" \
    make all
    echo "Build complete."
    if [[ -x "hwpc_cxi_sendrecv_test" ]]; then
        ls -lh hwpc_cxi_sendrecv_test | awk '{print "  Binary: " $9 " (" $5 ")"}'
    fi
    echo
fi

if [[ "$DO_RUN" == "true" ]]; then
    RUN_SCRIPT="$SCRIPT_DIR/run_hwpc_cxi_validate.sh"
    if [[ ! -x "$RUN_SCRIPT" ]]; then
        echo "ERROR: $RUN_SCRIPT is missing or not executable." >&2
        exit 1
    fi

    if [[ "$OMPI_PREFIX" == __REPLACE_ME_* ]] && ! command -v mpirun >/dev/null 2>&1; then
        echo "ERROR: mpirun not found in PATH, and OMPI_PREFIX is a placeholder." >&2
        echo "Set OMPI_PREFIX to your Open MPI install prefix or add mpirun to PATH." >&2
        exit 1
    fi

    echo "Running HWPC_CXI validation..."
    RUN_ARGS=()
    if [[ "$SAVE_BASELINE" == "true" ]]; then
        RUN_ARGS+=("--save-baseline")
    fi
    RUN_ARGS+=("$NUM_PROCS" "$NUM_PPN" "$LOOPS")

    "$RUN_SCRIPT" "${RUN_ARGS[@]}"
    echo "Validation run complete."
fi
