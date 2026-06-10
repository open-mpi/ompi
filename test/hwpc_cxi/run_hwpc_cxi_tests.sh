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

# Compatibility wrapper: the supported HWPC_CXI validation flow now runs
# through run_hwpc_cxi_validate.sh and hwpc_cxi_sendrecv_test.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "run_hwpc_cxi_tests.sh is deprecated."
echo "Forwarding to run_hwpc_cxi_validate.sh using hwpc_cxi_sendrecv_test."

exec "${SCRIPT_DIR}/run_hwpc_cxi_validate.sh" "$@"
