#!/bin/sh
set -eu

test_dir=$(cd "${1:-.}" && pwd)
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/hwpc-cxi-quiet.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

for program in hwpc_cxi_report_level_0_test hwpc_cxi_mpi_init_finalize_level_0_test; do
    if ! env OMPI_MCA_ompi_hook_hwpc_cxi_counter_verbose=true \
        OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file="$work_dir/report" \
        "$test_dir/$program" > "$work_dir/stdout" 2> "$work_dir/stderr"; then
        cat "$work_dir/stderr" >&2
        echo "FAIL: $program exited unsuccessfully" >&2
        exit 1
    fi
    # Only hwpc_cxi's own output counts: other components (e.g., hook/demo, mpiext/example) may print.
    if grep -E 'HWPC_CXI|Open MPI Slingshot' "$work_dir/stdout" "$work_dir/stderr" >&2; then
        echo "FAIL: $program emitted HWPC_CXI output in quiet mode" >&2
        exit 1
    fi
    for report in "$work_dir"/report*; do
        if test -e "$report"; then
            echo "FAIL: $program created a report in quiet mode" >&2
            exit 1
        fi
    done
done
