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
    cat "$work_dir/stderr" >&2
    awk '
        pending {
            if ((pending == 1 && $0 ~ /^[[:space:]]*pmix_mca_[[:space:]]*$/) ||
                (pending == 2 && $0 ~ /^[[:space:]]*or[[:space:]]*$/) ||
                (pending == 3 && $0 ~ /^[[:space:]]*libpmix_mca_[[:space:]]*$/)) {
                block = block $0 "\n"
                if (++pending == 4) {
                    pending = 0
                    block = ""
                }
                next
            }
            printf "%s", block
            pending = 0
            block = ""
        }
        /^\[[^][]+:[0-9]+\] mca:base:process_repository_item filename .*\/3rd-party\/openpmix\/src\/mca\/[[:alnum:]_]+\/\.libs\/libmca_[[:alnum:]_]+ has (bad|unexpected) prefix - expected:$/ {
            block = $0 "\n"
            pending = 1
            next
        }
        { print }
        END { printf "%s", block }
    ' "$work_dir/stderr" > "$work_dir/unexpected_stderr"
    if test -s "$work_dir/stdout" || test -s "$work_dir/unexpected_stderr"; then
        cat "$work_dir/stdout" >&2
        echo "FAIL: $program emitted output in quiet mode" >&2
        exit 1
    fi
    for report in "$work_dir"/report*; do
        if test -e "$report"; then
            echo "FAIL: $program created a report in quiet mode" >&2
            exit 1
        fi
    done
done