#!/bin/bash
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
source "${script_dir}/../test-multi/hwpc_cxi_compare.sh"
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/hwpc-cxi-compare.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT

compare_case() {
    local label="$1" expected="$2" baseline="$3" current="$4" require_data="${5:-1}" ignore_hostname="${6:-0}"
    printf '%s' "$baseline" > "$work_dir/baseline"
    printf '%s' "$current" > "$work_dir/current"
    local result=0
    if compare_counter_logs "$work_dir/baseline" "$work_dir/current" "$work_dir/result" 20 0 "$require_data" "$ignore_hostname"; then
        result=0
    else
        result=1
    fi
    if [[ "$result" != "$expected" ]]; then
        cat "$work_dir/result" >&2
        echo "FAIL: $label" >&2
        exit 1
    fi
}

detail=$'CXI_COUNTER_DATA hostname device counter_name delta rate\nCXI_COUNTER_DATA hostA 0 counter_x 10 2\n'
compare_case 'identical detail rows' 0 "$detail" "$detail"
compare_case 'empty current report' 1 "$detail" ''
compare_case 'empty baseline report' 1 '' "$detail"
compare_case 'empty reports' 1 '' ''
compare_case 'header-only reports' 1 $'CXI_COUNTER_DATA hostname device counter_name delta rate\n' $'CXI_COUNTER_DATA hostname device counter_name delta rate\n'
compare_case 'unrecognized contents' 1 $'baseline text\n' $'different text\n'
compare_case 'hostname change' 1 "$detail" $'CXI_COUNTER_DATA hostB 0 counter_x 10 2\n'
compare_case 'allocation-independent hostname comparison' 0 "$detail" $'CXI_COUNTER_DATA hostB 0 counter_x 10 2\n' 1 1
compare_case 'hostname normalization preserves device identity' 1 "$detail" $'CXI_COUNTER_DATA hostB 1 counter_x 10 2\n' 1 1
compare_case 'device change' 1 "$detail" $'CXI_COUNTER_DATA hostA 1 counter_x 10 2\n'
compare_case 'counter change' 1 "$detail" $'CXI_COUNTER_DATA hostA 0 counter_y 10 2\n'
compare_case 'malformed detail row' 1 "$detail" "$detail"$'CXI_COUNTER_DATA hostA 0 broken not_numeric 2\n'
compare_case 'numeric deviation remains a warning' 0 "$detail" $'CXI_COUNTER_DATA hostA 0 counter_x 100 20\n'
grep -q '^WARN ' "$work_dir/result"
summary=$'counter_x 1 10 2 10 2 10 2\n'
compare_case 'summary rows are not detail rows' 1 "$summary" "$summary"
compare_case 'stdout summary supported' 0 "$summary" "$summary" 0
compare_case 'missing summary counter' 1 "$summary" '' 0
compare_case 'empty summary logs allowed' 0 '' '' 0
printf 'ignored 1 1 1 1 1 1 1\nPE 0:\n%s' "$summary" > "$work_dir/baseline"
printf 'different 2 2 2 2 2 2 2\nPE 0:\n%s' "$summary" > "$work_dir/current"
compare_counter_logs "$work_dir/baseline" "$work_dir/current" "$work_dir/result" 20 1
printf '%s\n' 'PASS: counter comparison regressions'