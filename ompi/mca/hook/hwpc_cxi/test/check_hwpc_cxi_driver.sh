#!/bin/bash
set -euo pipefail

mock_srun() {
    local ranks=1 ppn=1 bounded=0
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --time=*) bounded=1; shift ;;
            --mpi=*) shift ;;
            -n) ranks="$2"; shift 2 ;;
            --ntasks-per-node) ppn="$2"; shift 2 ;;
            *) break ;;
        esac
    done
    (( bounded )) || { echo 'ERROR: launch has no time limit' >&2; return 9; }
    if [[ "${2:-}" == --mpi-case ]]; then
        printf 'PASS: MPI fixture %s\n' "$3"
        return 0
    fi
    local prefix="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file}"
    local level="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_report}"
    local input="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_file}"
    local fault="${HWPC_CXI_MOCK_FAILURE:-none}"
    case "$prefix" in
        */validate_sendrecv_*/report)
            [[ "$fault" != primary ]] || return 7
            [[ "$fault" != timeout ]] || return 124
            ;;
        *hwpc_cxi_report1) [[ "$fault" != auxiliary ]] || return 7 ;;
    esac
    if [[ "$fault" == placement ]]; then
        echo 'sendrecv placement: cross_node=0 peer=1' >&2
    else
        echo 'sendrecv placement: cross_node=1 peer=2' >&2
    fi
    echo 'hwpc_cxi_sendrecv_test complete elapsed_with_mpi_init_finalize=1.0 elapsed_without_mpi_init_finalize=0.5'
    if [[ -n "${HWPC_CXI_TEST_REPORT_MISMATCH:-}" ]]; then
        echo 'HWPC_CXI: WARNING: ranks disagreed on reporting level' >&2
        return 0
    elif [[ -n "${HWPC_CXI_TEST_ALTERNATE_COUNTER_FILE:-}" ]]; then
        echo 'HWPC_CXI: WARNING: node leaders disagreed on counter schema' >&2
        return 0
    fi
    [[ "$level" != 0 ]] || return 0
    if [[ "$level" == 1 && "$fault" == timeouts ]]; then
        echo 'OpenMPI Slingshot Network Summary: 3 network timeouts'
    elif [[ "$level" == 1 && "$fault" == zero_summary ]]; then
        echo 'OpenMPI Slingshot Network Summary: 0 network timeouts'
    fi
    if (( level >= 2 )); then
        echo 'OpenMPI Slingshot Network Summary: 0 network timeouts'
        echo 'OpenMPI Slingshot CXI Counter Summary:'
        echo 'counter_x 1 10 2 10 2 10 2'
    fi
    local hosts=$(( (ranks + ppn - 1) / ppn )) host token
    if [[ "$input" == *my_desired_fake_cxi_counters.txt ]]; then
        for (( host=0; host<hosts; ++host )); do
            while IFS= read -r token; do
                printf 'host%d: HWPC_CXI: WARNING: Token %s not recognized\n' "$host" "$token" >&2
            done < <(awk -v RS='[",[:space:]]+' 'index(tolower($0), "fake") {print}' "$input")
        done
    fi
    if [[ "$input" == */input_mixed_*/counters.txt ]]; then
        echo 'HWPC_CXI: INFO: Summary of valid counters detected from CXI counter file' >&2
    elif [[ "$input" == */input_* ]]; then
        echo 'HWPC_CXI: INFO: Summary of valid counters detected from the default CXI counters' >&2
    fi
    (( level >= 3 )) || return 0
    printf 'fixture descriptions\n' > "$prefix.counter_descriptions"
    if [[ "$fault" == missing && "$prefix" == *hwpc_cxi_report5 ]]; then
        return 0
    fi
    for (( host=0; host<hosts; ++host )); do
        local hostname="${HWPC_CXI_MOCK_HOST_PREFIX:-host}$host"
        local device
        {
            echo 'CXI_COUNTER_DATA hostname device counter_name delta rate'
            for (( device=0; device<2; ++device )); do
                printf 'CXI_COUNTER_DATA %s %d rh:sct_timeouts 0 0\n' "$hostname" "$device"
                printf 'CXI_COUNTER_DATA %s %d rh:spt_timeouts 0 0\n' "$hostname" "$device"
                if [[ "$fault" != mixed_missing_selected || "$input" != */input_mixed_*/counters.txt ]]; then
                    printf 'CXI_COUNTER_DATA %s %d lpe_rndzv_puts_0 10 2\n' "$hostname" "$device"
                fi
            done
            if [[ "$fault" == mixed_extra_counter && "$input" == */input_mixed_*/counters.txt ]]; then
                printf 'CXI_COUNTER_DATA %s 1 atu_cache_evictions 10 2\n' "$hostname"
            fi
            if [[ "$fault" == malformed && "$prefix" == *hwpc_cxi_report5 ]]; then
                echo 'CXI_COUNTER_DATA broken row'
            fi
        } > "$prefix.$hostname"
    done
}

mock_quiet_program() {
    local scenario="${HWPC_CXI_MOCK_QUIET:-silent}"
    local spelling=bad
    local framework_path=/home/users/test/hpc-openmpi/3rd-party/openpmix/src/mca/bfrops/.libs/libmca_bfrops
    case "$scenario" in
        silent) return 0 ;;
        stdout) echo 'unexpected stdout'; return 0 ;;
        pmix_error) echo 'PMIX ERROR: initialization failed' >&2; return 0 ;;
        unexpected) spelling=unexpected ;;
        foreign_path) framework_path=/opt/other/.libs/libmca_bfrops ;;
    esac
    printf '[portage-login1:322199] mca:base:process_repository_item filename %s has %s prefix - expected:\n' \
        "$framework_path" "$spelling" >&2
    printf '    pmix_mca_\nor\n' >&2
    if [[ "$scenario" == incomplete ]]; then
        return 0
    elif [[ "$scenario" == malformed ]]; then
        echo '    libpmix_mca_ ERROR: initialization failed' >&2
        return 0
    fi
    echo '    libpmix_mca_' >&2
    case "$scenario" in
        hwpc_output) echo 'HWPC_CXI: INFO: unexpected output' >&2 ;;
        extra_error) echo 'PMIX ERROR: initialization failed' >&2 ;;
        nonzero) return 7 ;;
        report) : > "$OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file" ;;
    esac
    return 0
}

if [[ "${0##*/}" == hwpc_cxi_report_level_0_test ||
      "${0##*/}" == hwpc_cxi_mpi_init_finalize_level_0_test ]]; then
    mock_quiet_program
    exit $?
fi

if [[ "${0##*/}" == srun ]]; then
    mock_srun "$@"
    exit $?
fi

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/hwpc-cxi-driver.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT
mkdir -p "$work_dir/bin"
cp "${BASH_SOURCE[0]}" "$work_dir/bin/srun"
chmod +x "$work_dir/bin/srun"
export PATH="$work_dir/bin:$PATH"
export HWPC_CXI_TEST_BIN="$work_dir/bin/srun"
export HWPC_CXI_FIXTURE_TEST_BIN="$work_dir/bin/srun"
unset HWPC_CXI_MPI_T_TEST_BIN HWPC_CXI_TEST_REPORT_MISMATCH HWPC_CXI_TEST_ALTERNATE_COUNTER_FILE
unset OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_file="$script_dir/../test-multi/my_desired_cxi_counters.txt"
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=5
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_verbose=true
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros=false
export HWPC_CXI_REQUIRE_CROSS_NODE=1
export HWPC_CXI_LOGS_DIR="$work_dir/logs"
driver="$script_dir/../test-multi/run_hwpc_cxi_validate.sh"

run_case() {
    local fault="$1" expected="$2"
    shift 2
    local result=0
    if HWPC_CXI_MOCK_FAILURE="$fault" bash "$driver" "$@" 4 2 1 > "$work_dir/output" 2>&1; then
        result=0
    else
        result=$?
    fi
    if { [[ "$expected" == pass ]] && (( result != 0 )); } || \
       { [[ "$expected" == fail ]] && (( result == 0 )); }; then
        cat "$work_dir/output" >&2
        echo "FAIL: driver scenario $fault (status $result)" >&2
        exit 1
    fi
}

run_case none pass --save-baseline
cp -R "$HWPC_CXI_LOGS_DIR/baseline" "$work_dir/saved_baseline"
export HWPC_CXI_MOCK_HOST_PREFIX=other
run_case none pass
run_case timeouts pass
for fault in primary auxiliary timeout malformed missing placement zero_summary mixed_extra_counter mixed_missing_selected; do
    run_case "$fault" fail --save-baseline
    if ! grep -q 'BASELINE: not saved because validation failed' "$work_dir/output"; then
        cat "$work_dir/output" >&2
        echo "FAIL: scenario $fault did not reach the baseline-save guard" >&2
        exit 1
    fi
    diff -r "$work_dir/saved_baseline" "$HWPC_CXI_LOGS_DIR/baseline"
    case "$fault" in
        primary) grep -q 'RESULT: FAIL (exit code 7)' "$work_dir/output" ;;
        timeout) grep -q 'RESULT: FAIL (exit code 124)' "$work_dir/output" ;;
        mixed_*) grep -q 'INPUT mixed: FAIL' "$work_dir/output" ;;
    esac
done
if bash "$driver" 0 2 1 > "$work_dir/output" 2>&1; then
    echo 'FAIL: driver accepted zero ranks' >&2
    exit 1
fi
for program in hwpc_cxi_report_level_0_test hwpc_cxi_mpi_init_finalize_level_0_test; do
    cp "${BASH_SOURCE[0]}" "$work_dir/bin/$program"
    chmod +x "$work_dir/bin/$program"
done
for scenario in silent bad unexpected incomplete malformed foreign_path stdout pmix_error hwpc_output extra_error nonzero report; do
    expected=1
    case "$scenario" in silent|bad|unexpected) expected=0 ;; esac
    result=0
    if HWPC_CXI_MOCK_QUIET="$scenario" sh "$script_dir/check_hwpc_cxi_quiet.sh" \
        "$work_dir/bin" > "$work_dir/quiet_output" 2>&1; then
        result=0
    else
        result=1
    fi
    if [[ "$result" != "$expected" ]]; then
        cat "$work_dir/quiet_output" >&2
        echo "FAIL: quiet scenario $scenario" >&2
        exit 1
    fi
    case "$scenario" in
        bad|unexpected) grep -q "has $scenario prefix - expected:" "$work_dir/quiet_output" ;;
    esac
done
printf '%s\n' 'PASS: quiet warning allowlist regressions'
printf '%s\n' 'PASS: validation driver regressions'