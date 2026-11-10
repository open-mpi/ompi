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
# Check the hwpc_cxi MCA parameters as ompi_info reports them: the report-level
# names, that a level is accepted by name, and the counter_file and
# counter_verbose help text.

set -u

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <ompi_info>" >&2
    exit 2
fi

ompi_info="$1"

if [ ! -x "$ompi_info" ]; then
    echo "SKIP: ompi_info is not built yet: $ompi_info"
    exit 0
fi

# Set the report level by name to check that it is accepted; ompi_info only registers components (it never opens them), so no counters are read.
if ! out=$(OMPI_MCA_hook_hwpc_cxi_counter_report=all-nics "$ompi_info" --parsable --param hook hwpc_cxi --level 9 2>&1); then
    echo "ERROR: ompi_info failed:" >&2
    printf '%s\n' "$out" >&2
    exit 1
fi

param="mca:hook:hwpc_cxi:param"
failures=0

expect() {
    if ! printf '%s\n' "$out" | grep -F -- "$1" >/dev/null; then
        echo "FAIL: $2 (missing: $1)" >&2
        failures=$((failures + 1))
    fi
}

expect "$param:hook_hwpc_cxi_counter_report:enumerator:value:0:quiet" "level 0 name"
expect "$param:hook_hwpc_cxi_counter_report:enumerator:value:1:timeouts" "level 1 name"
expect "$param:hook_hwpc_cxi_counter_report:enumerator:value:2:summary" "level 2 name"
expect "$param:hook_hwpc_cxi_counter_report:enumerator:value:3:timeout-nics" "level 3 name"
expect "$param:hook_hwpc_cxi_counter_report:enumerator:value:4:all-nics-on-timeout" "level 4 name"
expect "$param:hook_hwpc_cxi_counter_report:enumerator:value:5:all-nics" "level 5 name"
expect "$param:hook_hwpc_cxi_counter_report:value:all-nics" "report level accepted by name"
expect "$param:hook_hwpc_cxi_counter_file:help:Path to a file listing Cassini counter names, counter mnemonics, or counter groups (separated by commas, spaces, tabs, or newlines)" "counter_file help text"
expect "$param:hook_hwpc_cxi_counter_verbose:help:Print diagnostic messages about the Cassini counters being selected and collected" "counter_verbose help text"

if [ "$failures" -ne 0 ]; then
    echo "ompi_info output:" >&2
    printf '%s\n' "$out" >&2
    exit 1
fi

exit 0
