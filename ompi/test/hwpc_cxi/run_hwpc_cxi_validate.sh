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

# Run a minimal MPI send/recv workload for HWPC_CXI validation and capture:
#   1) test stdout
#   2) test stderr
#   3) HWPC_CXI report files matching OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file*
#
# Usage: run_hwpc_cxi_validate.sh [--save-baseline] [num_procs [num_ppn [loops]]]
#   --save-baseline  Save this run's outputs as the reference baseline for future
#                    comparisons.  Baseline stored in: validation_logs/baseline/
#                    Without this flag, any existing baseline is compared automatically.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

PASS_LABEL="PASS"
FAIL_LABEL="FAIL"
if [[ -t 1 && "${TERM:-}" != "dumb" ]]; then
    printf -v PASS_LABEL '\033[32mPASS\033[0m'
    printf -v FAIL_LABEL '\033[31mFAIL\033[0m'
fi

# Parse optional flags before positional arguments.
SAVE_BASELINE=false
while [[ $# -gt 0 && "$1" == --* ]]; do
    case "$1" in
        --save-baseline) SAVE_BASELINE=true ;;
        *) echo "ERROR: unknown option: $1" >&2; exit 1 ;;
    esac
    shift
done

NUM_PROCS="${1:-4}"
NUM_PPN="${2:-2}"
LOOPS="${3:-100}"

LOGS_DIR="${SCRIPT_DIR}/validation_logs"
BASELINE_DIR="${LOGS_DIR}/baseline"
RUN_TAG="validate_sendrecv_$(date +%Y%m%d_%H%M%S)"
RUN_DIR="${LOGS_DIR}/${RUN_TAG}"
mkdir -p "${RUN_DIR}"

# Set default HWPC_CXI environment values unless the caller already set them.
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_file="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_file:-${SCRIPT_DIR}/my_desired_cxi_counters.txt}"
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_report="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_report:-2}"
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros:-false}"
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_verbose="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_verbose:-true}"
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file:-${SCRIPT_DIR}/mock_report_prefix}"
#export OMPI_MCA_btl="${OMPI_MCA_btl:-^ofi}"

HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH:-}"
if [[ "${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH}" == __REPLACE_ME_* ]]; then
    HWPC_CXI_RUNTIME_LD_LIBRARY_PATH=""
fi
if [[ -n "${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH}" ]]; then
    export LD_LIBRARY_PATH="${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH:-}"
fi

TEST_BIN="${HWPC_CXI_TEST_BIN:-${SCRIPT_DIR}/hwpc_cxi_sendrecv_test}"
STDOUT_LOG="${RUN_DIR}/stdout.log"
STDERR_LOG="${RUN_DIR}/stderr.log"

if [[ ! -x "${TEST_BIN}" ]]; then
    echo "ERROR: ${TEST_BIN} is missing or not executable. Build tests first with 'make -C test/hwpc_cxi'." >&2
    exit 1
fi

REPORT_PREFIX="${OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file}"
REPORT_DIR="$(dirname "${REPORT_PREFIX}")"
REPORT_BASE="$(basename "${REPORT_PREFIX}")"
if [[ "${REPORT_BASE}" == *"*"* || "${REPORT_BASE}" == *"?"* || "${REPORT_BASE}" == *"["* ]]; then
    echo "ERROR: report prefix basename must not contain glob metacharacters '*', '?', or '[': ${REPORT_BASE}" >&2
    exit 1
fi
[[ "${REPORT_DIR}" == "." ]] && REPORT_DIR="${SCRIPT_DIR}"

mkdir -p "${REPORT_DIR}"

# Compare HWPC_CXI counter logs by counter name and numeric values.
# Missing counter names are errors. Numeric deviations > tolerance are warnings.
# Optional 5th argument: pe0_gate=1 activates PE 0: gating – lines before the
# first "PE 0:" line are ignored except those containing the word "error".
# Default (pe0_gate=0): all counter lines are compared regardless of position.
compare_counter_logs() {
    local baseline_log="$1"
    local current_log="$2"
    local output_log="$3"
    local tolerance_pct="$4"
    local pe0_gate="${5:-0}"

    # Pre-detect PE 0: presence only when gating is requested.
    local base_has_pe0=0 curr_has_pe0=0
    if [[ "${pe0_gate}" == "1" ]]; then
        grep -q '^PE 0:' "${baseline_log}" 2>/dev/null && base_has_pe0=1
        grep -q '^PE 0:' "${current_log}"  2>/dev/null && curr_has_pe0=1
    fi

    awk -v tolerance_pct="${tolerance_pct}" \
        -v base_has_pe0="${base_has_pe0}" \
        -v curr_has_pe0="${curr_has_pe0}" \
    '
        function is_num(v) {
            return (v ~ /^[-+]?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$/)
        }
        function abs(v) {
            return v < 0 ? -v : v
        }
        function parse_counter_line() {
            if (NF == 6 && $1 == "CXI_COUNTER_DATA" && $2 != "hostname" &&
                is_num($3) && is_num($5) && is_num($6)) {
                parsed_key = "detail" SUBSEP $2 SUBSEP $3 SUBSEP $4
                parsed_label = $2 "/" $3 "/" $4
                parsed_start = 5
                parsed_count = 2
                return 1
            }
            if (NF >= 8 && $1 != "Counter" && is_num($2) && is_num($3)) {
                parsed_key = "summary" SUBSEP $1
                parsed_label = $1
                parsed_start = 2
                parsed_count = NF - 1
                return 1
            }
            return 0
        }
        BEGIN {
            err = 0
            warn = 0
            n_base = 0
            n_curr = 0
            base_active = 0
            curr_active = 0
        }
        FNR == NR {
            if (tolower($0) ~ /error/) {
                print "ERROR baseline line: " $0
                err++
                next
            }
            # Activate once "PE 0:" is seen (only when the file has such lines).
            if (base_has_pe0 && $0 ~ /^PE 0:/) {
                base_active = 1
            }
            # Before the activation point: skip non-counter content.
            if (base_has_pe0 && !base_active) {
                next
            }
            if (!parse_counter_line()) {
                next
            }
            name = parsed_key
            if (!(name in seen_base)) {
                seen_base[name] = 1
                base_names[++n_base] = name
                base_label[name] = parsed_label
            }
            base_count[name] = parsed_count
            for (i = 0; i < parsed_count; ++i) {
                base_val[name, i + 1] = $(parsed_start + i)
            }
            next
        }
        {
            if (tolower($0) ~ /error/) {
                print "ERROR current line: " $0
                err++
                next
            }
            # Activate once "PE 0:" is seen (only when the file has such lines).
            if (curr_has_pe0 && $0 ~ /^PE 0:/) {
                curr_active = 1
            }
            # Before the activation point: skip non-counter content.
            if (curr_has_pe0 && !curr_active) {
                next
            }
            if (!parse_counter_line()) {
                next
            }
            name = parsed_key
            if (!(name in seen_curr)) {
                seen_curr[name] = 1
                curr_names[++n_curr] = name
                curr_label[name] = parsed_label
            }
            curr_count[name] = parsed_count
            for (i = 0; i < parsed_count; ++i) {
                curr_val[name, i + 1] = $(parsed_start + i)
            }
        }
        END {
            print "Counter comparison (tolerance=" tolerance_pct "%)"
            for (idx = 1; idx <= n_base; ++idx) {
                name = base_names[idx]
                label = base_label[name]
                if (!(name in seen_curr)) {
                    print "ERROR missing counter in current log: " label
                    err++
                    continue
                }

                if (base_count[name] != curr_count[name]) {
                    print "ERROR counter column mismatch for " label ": baseline has " base_count[name] ", current has " curr_count[name]
                    err++
                    continue
                }

                for (i = 1; i <= base_count[name]; ++i) {
                    b = base_val[name, i]
                    c = curr_val[name, i]
                    if (!is_num(b) || !is_num(c)) {
                        print "ERROR non-numeric field for counter " label " column " i ": baseline='" b "' current='" c "'"
                        err++
                        continue
                    }

                    if ((b + 0) == 0) {
                        pct = ((c + 0) == 0) ? 0 : 100
                    } else {
                        pct = (abs((c + 0) - (b + 0)) / abs(b + 0)) * 100
                    }

                    if (pct > tolerance_pct) {
                        warn++
                        printf("WARN %s col%d baseline=%s current=%s delta=%.2f%%\n", label, i, b, c, pct)
                    }
                }
            }

            for (idx = 1; idx <= n_curr; ++idx) {
                name = curr_names[idx]
                if (!(name in seen_base)) {
                    print "ERROR new counter in current log (not in baseline): " curr_label[name]
                    err++
                }
            }

            print "Summary: warnings=" warn ", errors=" err
            exit err
        }
    ' "${baseline_log}" "${current_log}" > "${output_log}"
}

# Fail when stdout contains numeric-heavy lines where every numeric field is zero.
# A numeric-heavy line is defined as having at least 3 numeric tokens.
check_stdout_for_all_zero_numeric_lines() {
    local stdout_log="$1"
    local output_log="$2"

    awk '
        function is_num(v) {
            return (v ~ /^[-+]?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$/)
        }
        {
            n_numeric = 0
            n_nonzero = 0
            for (i = 1; i <= NF; ++i) {
                if (is_num($i)) {
                    n_numeric++
                    if (($i + 0) != 0) {
                        n_nonzero++
                    }
                }
            }

            if (n_numeric >= 3 && n_nonzero == 0) {
                printf("line %d: %s\n", NR, $0)
                bad = 1
            }
        }
        END {
            exit bad ? 1 : 0
        }
    ' "${stdout_log}" > "${output_log}"
}

# Start from a clean slate for this prefix so captured files are from this run.
find "${REPORT_DIR}" -maxdepth 1 -type f -name "${REPORT_BASE}*" -print -delete > "${RUN_DIR}/preexisting_report_files_removed.txt" 2>/dev/null || true

echo "========================================================"
echo "HWPC_CXI send/recv validation run"
echo "========================================================"
echo "run_dir:        ${RUN_DIR}"
echo "num_procs:      ${NUM_PROCS}"
echo "num_ppn:        ${NUM_PPN}"
echo "loops:          ${LOOPS}"
echo "report_prefix:  ${REPORT_PREFIX}"
echo "report_dir:     ${REPORT_DIR}"
echo "========================================================"

# Run the test and capture stdout / stderr separately.
rc=0
if srun --mpi=pmix -n "${NUM_PROCS}"  --ntasks-per-node "${NUM_PPN}" "${TEST_BIN}" "${LOOPS}" > "${STDOUT_LOG}" 2> "${STDERR_LOG}"; then
    echo "RESULT: ${PASS_LABEL}"
else
    rc=$?
    echo "RESULT: ${FAIL_LABEL} (exit code ${rc})"
fi

# Capture any report files matching the configured prefix.
shopt -s nullglob
report_matches=("${REPORT_DIR}/${REPORT_BASE}"*)
shopt -u nullglob

if (( ${#report_matches[@]} > 0 )); then
    REPORT_CAPTURE_DIR="${RUN_DIR}/reports"
    mkdir -p "${REPORT_CAPTURE_DIR}"
    for report_file in "${report_matches[@]}"; do
        cp -f "${report_file}" "${REPORT_CAPTURE_DIR}/"
    done
    printf '%s\n' "${report_matches[@]}" > "${RUN_DIR}/captured_report_files.txt"
else
    : > "${RUN_DIR}/captured_report_files.txt"
fi

echo "stdout_log:     ${STDOUT_LOG}"
echo "stderr_log:     ${STDERR_LOG}"
echo "report_index:   ${RUN_DIR}/captured_report_files.txt"
if [[ -d "${RUN_DIR}/reports" ]]; then
    echo "reports_dir:    ${RUN_DIR}/reports"
else
    echo "reports_dir:    (none found)"
fi

# ── Baseline save / compare ──────────────────────────────────────────────────
if $SAVE_BASELINE; then
    echo "BASELINE: save requested; waiting for all validation checks"
elif [[ -d "${BASELINE_DIR}" ]]; then
    echo "======================================================="
    echo "COMPARISON against baseline: ${BASELINE_DIR}"
    echo "======================================================="
    _compare_pass=true

    # stdout counter lines: allow numeric variance within tolerance, warn above it,
    # but fail when baseline counter names are missing.
    if compare_counter_logs "${BASELINE_DIR}/stdout.log" "${STDOUT_LOG}" "${RUN_DIR}/stdout_counter_compare.txt" 20; then
        echo "stdout:          MATCH (see ${RUN_DIR}/stdout_counter_compare.txt)"
    else
        echo "stdout:          ERROR (missing/invalid counters) -> ${RUN_DIR}/stdout_counter_compare.txt"
        _compare_pass=false
        rc=1
    fi

    # stderr: apply PE 0: gating so pre-activation preamble differences are
    # ignored; only error-containing lines before PE 0: are flagged.
    if compare_counter_logs "${BASELINE_DIR}/stderr.log" "${STDERR_LOG}" "${RUN_DIR}/stderr_counter_compare.txt" 20 1; then
        echo "stderr:          MATCH (see ${RUN_DIR}/stderr_counter_compare.txt)"
    else
        echo "stderr:          ERROR (missing/invalid counters) -> ${RUN_DIR}/stderr_counter_compare.txt"
        _compare_pass=false
        rc=1
    fi

    # report files
    if [[ -d "${BASELINE_DIR}/reports" ]]; then
        shopt -s nullglob
        for _brpt in "${BASELINE_DIR}/reports/"*; do
            _rname="$(basename "${_brpt}")"
            _crpt="${RUN_DIR}/reports/${_rname}"
            if [[ -f "${_crpt}" ]]; then
                if compare_counter_logs "${_brpt}" "${_crpt}" "${RUN_DIR}/${_rname}_counter_compare.txt" 20; then
                    echo "report ${_rname}: MATCH (see ${RUN_DIR}/${_rname}_counter_compare.txt)"
                else
                    echo "report ${_rname}: ERROR (missing/invalid counters) -> ${RUN_DIR}/${_rname}_counter_compare.txt"
                    _compare_pass=false
                    rc=1
                fi
            else
                echo "report ${_rname}: MISSING in current run"
                _compare_pass=false
                rc=1
            fi
        done
        if [[ -d "${RUN_DIR}/reports" ]]; then
            for _crpt in "${RUN_DIR}/reports/"*; do
                _rname="$(basename "${_crpt}")"
                if [[ ! -f "${BASELINE_DIR}/reports/${_rname}" ]]; then
                    echo "report ${_rname}: NEW (not in baseline)"
                    _compare_pass=false
                    rc=1
                fi
            done
        fi
        shopt -u nullglob
    fi

    if $_compare_pass; then
        echo "COMPARISON:      ${PASS_LABEL}"
    else
        echo "COMPARISON:      ${FAIL_LABEL}"
    fi
    echo "======================================================="
else
    echo "(no baseline found; run with --save-baseline to create one)"
fi
# ─────────────────────────────────────────────────────────────────────────────

# Run one validation scenario with its own environment and captured logs.
run_validation_scenario() {
    local run_dir="$1"
    local report_pattern="$2"
    local stdout_log="$3"
    local stderr_log="$4"
    shift 4

    find "${run_dir}" -maxdepth 1 -type f -name "${report_pattern}" -delete 2>/dev/null || true
    env "$@" srun --mpi=pmix -n "${NUM_PROCS}" --ntasks-per-node "${NUM_PPN}" "${TEST_BIN}" "${LOOPS}" \
        > "${stdout_log}" 2> "${stderr_log}"
}

# ── Fake-counter existence test ───────────────────────────────────────────────
# Run the same workload with my_desired_fake_cxi_counters.txt and verify that
# hwpc_cxi reports that the bogus counter names do not exist in stderr.
FAKE_COUNTER_FILE="${SCRIPT_DIR}/my_desired_fake_cxi_counters.txt"
echo "========================================================"
echo "FAKE-COUNTER test (expect 'not found' diagnostics in stderr)"
echo "  counter_file: ${FAKE_COUNTER_FILE}"
echo "========================================================"

FAKE_RUN_DIR="${LOGS_DIR}/fake_counter_${RUN_TAG}"
mkdir -p "${FAKE_RUN_DIR}"
FAKE_STDOUT_LOG="${FAKE_RUN_DIR}/stdout.log"
FAKE_STDERR_LOG="${FAKE_RUN_DIR}/stderr.log"
FAKE_REPORT_PREFIX="${FAKE_RUN_DIR}/hwpc_cxi_fake_report"

_fake_rc=0
if ! run_validation_scenario "${FAKE_RUN_DIR}" "hwpc_cxi_fake_report*" \
    "${FAKE_STDOUT_LOG}" "${FAKE_STDERR_LOG}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_file=${FAKE_COUNTER_FILE}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file=${FAKE_REPORT_PREFIX}"; then
    :
else
    _fake_rc=$?
fi

echo "fake_stdout_log: ${FAKE_STDOUT_LOG}"
echo "fake_stderr_log: ${FAKE_STDERR_LOG}"

if (( _fake_rc != 0 )); then
    echo "FAKE-COUNTER:    ${FAIL_LABEL} (workload exited with status ${_fake_rc})"
    echo "  Inspect: ${FAKE_STDERR_LOG}"
    rc=1
else
    # Bogus tokens contain "fake" so their count can be derived from the input.
    # Each local root is expected to emit one diagnostic for each bogus token.
    _fake_counter_count=$(awk -v RS='[",[:space:]]+' '
        index(tolower($0), "fake") { count++ }
        END { print count + 0 }
    ' "${FAKE_COUNTER_FILE}")
    _fake_node_count=$(( (NUM_PROCS + NUM_PPN - 1) / NUM_PPN ))
    _fake_expected_diag_count=$(( _fake_counter_count * _fake_node_count ))

    _fake_diag_count=$(grep -ciE \
        'does not exist|not found|not recognized|unrecognized|unknown counter|invalid counter' \
        "${FAKE_STDERR_LOG}" 2>/dev/null || true)

    if (( _fake_diag_count == _fake_expected_diag_count )); then
        echo "FAKE-COUNTER:    ${PASS_LABEL} (${_fake_diag_count} diagnostic line(s) found in stderr)"
    else
        echo "FAKE-COUNTER:    ${FAIL_LABEL} (${_fake_diag_count} diagnostic line(s) found in stderr; expected ${_fake_expected_diag_count} = ${_fake_counter_count} fake counter(s) x ${_fake_node_count} node(s))"
        echo "  Expected hwpc_cxi to report unrecognised counter names."
        echo "  Inspect: ${FAKE_STDERR_LOG}"
        rc=1
    fi
fi
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────

# ── Report-level=1 timeout-summary test ──────────────────────────────────────
# Run with the normal desired counter file, but force report level 1.
# Expected stdout behavior: no CXI counter summary table; only timeout summary
# should be emitted by HWPC_CXI.
REPORT1_COUNTER_FILE="${SCRIPT_DIR}/my_desired_cxi_counters.txt"
echo "========================================================"
echo "REPORT_LEVEL=1 test (timeout summary only)"
echo "  counter_file: ${REPORT1_COUNTER_FILE}"
echo "========================================================"

REPORT1_RUN_DIR="${LOGS_DIR}/report_level_1_${RUN_TAG}"
mkdir -p "${REPORT1_RUN_DIR}"
REPORT1_STDOUT_LOG="${REPORT1_RUN_DIR}/stdout.log"
REPORT1_STDERR_LOG="${REPORT1_RUN_DIR}/stderr.log"
REPORT1_REPORT_PREFIX="${REPORT1_RUN_DIR}/hwpc_cxi_report1"

_report1_rc=0
if ! run_validation_scenario "${REPORT1_RUN_DIR}" "hwpc_cxi_report1*" \
    "${REPORT1_STDOUT_LOG}" "${REPORT1_STDERR_LOG}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_file=${REPORT1_COUNTER_FILE}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=1" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file=${REPORT1_REPORT_PREFIX}"; then
    :
else
    _report1_rc=$?
fi

echo "report1_stdout_log: ${REPORT1_STDOUT_LOG}"
echo "report1_stderr_log: ${REPORT1_STDERR_LOG}"

if [[ "${_report1_rc}" -ne 0 ]]; then
    echo "REPORT_LEVEL=1:        ${FAIL_LABEL} (exit code ${_report1_rc})"
    rc=1
else
    if grep -qE 'OpenMPI Slingshot Network Summary: [0-9]+ network timeouts' "${REPORT1_STDOUT_LOG}"; then
        echo "REPORT_LEVEL=1 stdout: ${PASS_LABEL} (network timeout summary present)"
    else
        echo "REPORT_LEVEL=1:        ${FAIL_LABEL} (stdout is missing network timeout summary)"
        echo "  Inspect: ${REPORT1_STDOUT_LOG}"
        rc=1
    fi

    # REPORT_LEVEL=1 must not emit the detailed CXI counter summary table/header.
    if grep -q "OpenMPI Slingshot CXI Counter Summary:" "${REPORT1_STDOUT_LOG}"; then
        echo "REPORT_LEVEL=1:        ${FAIL_LABEL} (stdout contains CXI Counter Summary table)"
        echo "  Inspect: ${REPORT1_STDOUT_LOG}"
        rc=1
    else
        echo "REPORT_LEVEL=1 stdout: ${PASS_LABEL} (no detailed counter summary table)"
    fi

    # Compare stderr against baseline stderr when available.
    if [[ -f "${BASELINE_DIR}/stderr.log" ]]; then
        if compare_counter_logs "${BASELINE_DIR}/stderr.log" "${REPORT1_STDERR_LOG}" "${REPORT1_RUN_DIR}/stderr_vs_baseline.txt" 20 1; then
            echo "REPORT_LEVEL=1 stderr: ${PASS_LABEL} (see ${REPORT1_RUN_DIR}/stderr_vs_baseline.txt)"
        else
            echo "REPORT_LEVEL=1 stderr: ${FAIL_LABEL} (see ${REPORT1_RUN_DIR}/stderr_vs_baseline.txt)"
            rc=1
        fi
    else
        echo "REPORT_LEVEL=1 stderr: SKIP (no baseline stderr at ${BASELINE_DIR}/stderr.log)"
    fi
fi
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────


# ── Report-level=0 disabled-feature test ────────────────────────────────────
# Run with the normal desired counter file, but force report level 0.
# Expected behavior: HWPC_CXI feature is skipped entirely; no HWPC_CXI output
# is emitted to either stdout or stderr.
REPORT0_COUNTER_FILE="${SCRIPT_DIR}/my_desired_cxi_counters.txt"
echo "========================================================"
echo "REPORT_LEVEL=0 test (HWPC_CXI fully disabled)"
echo "  counter_file: ${REPORT0_COUNTER_FILE}"
echo "========================================================"

REPORT0_RUN_DIR="${LOGS_DIR}/report_level_0_${RUN_TAG}"
mkdir -p "${REPORT0_RUN_DIR}"
REPORT0_STDOUT_LOG="${REPORT0_RUN_DIR}/stdout.log"
REPORT0_STDERR_LOG="${REPORT0_RUN_DIR}/stderr.log"
REPORT0_REPORT_PREFIX="${REPORT0_RUN_DIR}/hwpc_cxi_report0"

_report0_rc=0
if ! run_validation_scenario "${REPORT0_RUN_DIR}" "hwpc_cxi_report0*" \
    "${REPORT0_STDOUT_LOG}" "${REPORT0_STDERR_LOG}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_file=${REPORT0_COUNTER_FILE}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=0" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file=${REPORT0_REPORT_PREFIX}"; then
    :
else
    _report0_rc=$?
fi

echo "report0_stdout_log: ${REPORT0_STDOUT_LOG}"
echo "report0_stderr_log: ${REPORT0_STDERR_LOG}"

if [[ "${_report0_rc}" -ne 0 ]]; then
    echo "REPORT=0:        ${FAIL_LABEL} (exit code ${_report0_rc})"
    rc=1
else
    # For REPORT=0, stdout may be empty or contain only the sendrecv completion
    # line. No HWPC_CXI summary/counter output should appear.
    if grep -qE 'HWPC_CXI|OpenMPI Slingshot' "${REPORT0_STDOUT_LOG}"; then
        echo "REPORT_LEVEL=0 stdout: ${FAIL_LABEL} (found HWPC_CXI output)"
        echo "  Inspect: ${REPORT0_STDOUT_LOG}"
        rc=1
    else
        mapfile -t _report0_stdout_lines < <(grep -v '^[[:space:]]*$' "${REPORT0_STDOUT_LOG}" || true)
        _report0_completion_re='^hwpc_cxi_sendrecv_test complete elapsed_with_mpi_init_finalize=[0-9]+([.][0-9]+)? elapsed_without_mpi_init_finalize=[0-9]+([.][0-9]+)?$'
        if (( ${#_report0_stdout_lines[@]} == 0 )); then
            echo "REPORT_LEVEL=0 stdout: ${PASS_LABEL} (empty)"
        elif (( ${#_report0_stdout_lines[@]} == 1 )) && [[ "${_report0_stdout_lines[0]}" =~ ${_report0_completion_re} ]]; then
            echo "REPORT_LEVEL=0 stdout: ${PASS_LABEL} (single completion line)"
        else
            echo "REPORT_LEVEL=0 stdout: ${FAIL_LABEL} (unexpected stdout content)"
            echo "  Inspect: ${REPORT0_STDOUT_LOG}"
            rc=1
        fi
    fi

    if grep -qiE 'HWPC_CXI|OpenMPI Slingshot' "${REPORT0_STDERR_LOG}"; then
        echo "REPORT_LEVEL=0 stderr: ${FAIL_LABEL} (found HWPC_CXI output)"
        echo "  Inspect: ${REPORT0_STDERR_LOG}"
        rc=1
    else
        echo "REPORT_LEVEL=0 stderr: ${PASS_LABEL} (no HWPC_CXI output)"
    fi
fi
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────

# ── Report-level=2 with verbose=false stderr-silence test ─────────────────
# Run with the normal desired counter file, force report level 2, and disable
# verbose logging. Expected behavior: no HWPC_CXI output is written to stderr.
REPORT2_COUNTER_FILE="${SCRIPT_DIR}/my_desired_cxi_counters.txt"
echo "========================================================"
echo "REPORT_LEVEL=2 + VERBOSE=false test (quiet stderr)"
echo "  counter_file: ${REPORT2_COUNTER_FILE}"
echo "========================================================"

REPORT2_RUN_DIR="${LOGS_DIR}/report_level_2_noVerbose_${RUN_TAG}"
mkdir -p "${REPORT2_RUN_DIR}"
REPORT2_STDOUT_LOG="${REPORT2_RUN_DIR}/stdout.log"
REPORT2_STDERR_LOG="${REPORT2_RUN_DIR}/stderr.log"
REPORT2_REPORT_PREFIX="${REPORT2_RUN_DIR}/hwpc_cxi_report2_noVerbose"

_report2_rc=0
if ! run_validation_scenario "${REPORT2_RUN_DIR}" "hwpc_cxi_report2_noVerbose*" \
    "${REPORT2_STDOUT_LOG}" "${REPORT2_STDERR_LOG}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_file=${REPORT2_COUNTER_FILE}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=2" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_verbose=false" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file=${REPORT2_REPORT_PREFIX}"; then
    :
else
    _report2_rc=$?
fi

echo "report2_stdout_log: ${REPORT2_STDOUT_LOG}"
echo "report2_stderr_log: ${REPORT2_STDERR_LOG}"

if [[ "${_report2_rc}" -ne 0 ]]; then
    echo "REPORT_LEVEL=2+VERBOSE=false: ${FAIL_LABEL} (exit code ${_report2_rc})"
    rc=1
else
    if grep -qiE 'HWPC_CXI|OpenMPI Slingshot' "${REPORT2_STDERR_LOG}"; then
        echo "REPORT_LEVEL=2+VERBOSE=false stderr: ${FAIL_LABEL} (found HWPC_CXI output)"
        echo "  Inspect: ${REPORT2_STDERR_LOG}"
        rc=1
    else
        echo "REPORT_LEVEL=2+VERBOSE=false stderr: ${PASS_LABEL} (no HWPC_CXI output)"
    fi
fi
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────

# ── Report-level=2 with verbose=false filter_zeros=true stderr-silence test ─────────────────
# Run with the normal desired counter file, force report level 2, and disable
# verbose logging. Expected behavior: no HWPC_CXI output is written to stderr.
REPORT2_COUNTER_FILE="${SCRIPT_DIR}/my_desired_cxi_counters.txt"
echo "========================================================"
echo "REPORT_LEVEL=2 + VERBOSE=false + FILTER_ZEROS=true test (quiet stderr)"
echo "  counter_file: ${REPORT2_COUNTER_FILE}"
echo "========================================================"

REPORT2_RUN_DIR="${LOGS_DIR}/report_level_2_noVerbose_noZeros_${RUN_TAG}"
mkdir -p "${REPORT2_RUN_DIR}"
REPORT2_STDOUT_LOG="${REPORT2_RUN_DIR}/stdout.log"
REPORT2_STDERR_LOG="${REPORT2_RUN_DIR}/stderr.log"
REPORT2_REPORT_PREFIX="${REPORT2_RUN_DIR}/hwpc_cxi_report2_noVerbose_noZeros"

_report2_rc=0
if ! run_validation_scenario "${REPORT2_RUN_DIR}" "hwpc_cxi_report2_noVerbose_noZeros*" \
    "${REPORT2_STDOUT_LOG}" "${REPORT2_STDERR_LOG}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_file=${REPORT2_COUNTER_FILE}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=2" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_verbose=false" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros=true" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file=${REPORT2_REPORT_PREFIX}"; then
    :
else
    _report2_rc=$?
fi

echo "report2_stdout_log: ${REPORT2_STDOUT_LOG}"
echo "report2_stderr_log: ${REPORT2_STDERR_LOG}"

if check_stdout_for_all_zero_numeric_lines "${REPORT2_STDOUT_LOG}" "${REPORT2_RUN_DIR}/stdout_all_zero_numeric_lines.txt"; then
    echo "REPORT_LEVEL=2+VERBOSE=false+FILTER_ZEROS=true zero-numeric check: ${PASS_LABEL}"
else
    echo "REPORT_LEVEL=2+VERBOSE=false+FILTER_ZEROS=true zero-numeric check: ${FAIL_LABEL} (found all-zero numeric-heavy line(s))"
    echo "  Inspect: ${REPORT2_RUN_DIR}/stdout_all_zero_numeric_lines.txt"
    rc=1
fi

if [[ "${_report2_rc}" -ne 0 ]]; then
    echo "REPORT_LEVEL=2+VERBOSE=false+FILTER_ZEROS=true: ${FAIL_LABEL} (exit code ${_report2_rc})"
    rc=1
else
    if grep -qiE 'HWPC_CXI|OpenMPI Slingshot' "${REPORT2_STDERR_LOG}"; then
        echo "REPORT_LEVEL=2+VERBOSE=false+FILTER_ZEROS=true stderr: ${FAIL_LABEL} (found HWPC_CXI output)"
        echo "  Inspect: ${REPORT2_STDERR_LOG}"
        rc=1
    else
        echo "REPORT_LEVEL=2+VERBOSE=false+FILTER_ZEROS=true stderr: ${PASS_LABEL} (no HWPC_CXI output)"
    fi
fi
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────

# ── Report-level=5 per-node report-file test ────────────────────────────────
# Run with the normal desired counter file, but force report level 5.
# Expected behavior: one report file per node using the configured prefix,
# with each file containing CXI_COUNTER_DATA lines for tracked counters.
REPORT5_COUNTER_FILE="${SCRIPT_DIR}/my_desired_cxi_counters.txt"
echo "========================================================"
echo "REPORT_LEVEL=5 test (per-node report files)"
echo "  counter_file: ${REPORT5_COUNTER_FILE}"
echo "========================================================"

REPORT5_RUN_DIR="${LOGS_DIR}/report_level_5_${RUN_TAG}"
mkdir -p "${REPORT5_RUN_DIR}"
REPORT5_STDOUT_LOG="${REPORT5_RUN_DIR}/stdout.log"
REPORT5_STDERR_LOG="${REPORT5_RUN_DIR}/stderr.log"
REPORT5_REPORT_PREFIX="${REPORT5_RUN_DIR}/hwpc_cxi_report5"
REPORT5_EXPECTED_HOSTS="$(( (NUM_PROCS + NUM_PPN - 1) / NUM_PPN ))"

_report5_rc=0
if ! run_validation_scenario "${REPORT5_RUN_DIR}" "hwpc_cxi_report5*" \
    "${REPORT5_STDOUT_LOG}" "${REPORT5_STDERR_LOG}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_file=${REPORT5_COUNTER_FILE}" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=5" \
    "OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file=${REPORT5_REPORT_PREFIX}"; then
    :
else
    _report5_rc=$?
fi

echo "report5_stdout_log: ${REPORT5_STDOUT_LOG}"
echo "report5_stderr_log: ${REPORT5_STDERR_LOG}"

shopt -s nullglob
_report5_files=("${REPORT5_REPORT_PREFIX}".*)
shopt -u nullglob
_report5_host_files=()
_report5_description_files=()
for _report5_file in "${_report5_files[@]}"; do
    if [[ "${_report5_file}" == "${REPORT5_REPORT_PREFIX}.counter_descriptions" ]]; then
        _report5_description_files+=("${_report5_file}")
    else
        _report5_host_files+=("${_report5_file}")
    fi
done

printf '%s\n' "${_report5_files[@]}" > "${REPORT5_RUN_DIR}/report_files.txt"
echo "report5_file_index: ${REPORT5_RUN_DIR}/report_files.txt"

if [[ "${_report5_rc}" -ne 0 ]]; then
    echo "REPORT_LEVEL=5 launch: ${FAIL_LABEL} (exit code ${_report5_rc})"
    rc=1
else
    echo "REPORT_LEVEL=5 launch: ${PASS_LABEL}"
fi

if (( ${#_report5_description_files[@]} != 1 )) || [[ ! -s "${_report5_description_files[0]:-}" ]]; then
    echo "REPORT_LEVEL=5 descriptions: ${FAIL_LABEL} (expected one nonempty descriptions file, found ${#_report5_description_files[@]})"
    rc=1
else
    echo "REPORT_LEVEL=5 descriptions: ${PASS_LABEL}"
fi

if (( ${#_report5_host_files[@]} != REPORT5_EXPECTED_HOSTS )); then
    echo "REPORT_LEVEL=5 host files: ${FAIL_LABEL} (expected ${REPORT5_EXPECTED_HOSTS}, found ${#_report5_host_files[@]})"
    rc=1
else
    echo "REPORT_LEVEL=5 host files: ${PASS_LABEL} (${#_report5_host_files[@]} file(s))"
fi

for _report5_file in "${_report5_host_files[@]}"; do
    if awk '
        function is_num(value) {
            return value ~ /^[-+]?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$/
        }
        $0 == "CXI_COUNTER_DATA hostname device counter_name delta rate" {
            header = 1
            next
        }
        $1 == "CXI_COUNTER_DATA" {
            if (NF != 6 || !is_num($3) || !is_num($5) || !is_num($6)) {
                bad = 1
            } else {
                data++
            }
        }
        END { exit !(header && data > 0 && !bad) }
    ' "${_report5_file}"; then
        echo "REPORT_LEVEL=5 data: ${PASS_LABEL} (${_report5_file})"
    else
        echo "REPORT_LEVEL=5 data: ${FAIL_LABEL} (${_report5_file})"
        rc=1
    fi
done
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────

if $SAVE_BASELINE; then
    echo "======================================================="
    if [[ "${rc}" -ne 0 ]]; then
        echo "BASELINE: not saved because validation failed"
    else
        BASELINE_STAGE_DIR="${LOGS_DIR}/.baseline.${RUN_TAG}"
        BASELINE_BACKUP_DIR="${LOGS_DIR}/.baseline.previous.${RUN_TAG}"

        rm -rf "${BASELINE_STAGE_DIR}" "${BASELINE_BACKUP_DIR}"
        mkdir -p "${BASELINE_STAGE_DIR}"
        cp "${STDOUT_LOG}" "${BASELINE_STAGE_DIR}/stdout.log"
        cp "${STDERR_LOG}" "${BASELINE_STAGE_DIR}/stderr.log"
        if [[ -d "${RUN_DIR}/reports" ]]; then
            mkdir -p "${BASELINE_STAGE_DIR}/reports"
            cp -f "${RUN_DIR}/reports/"* "${BASELINE_STAGE_DIR}/reports/"
        fi

        if [[ -e "${BASELINE_DIR}" ]]; then
            mv "${BASELINE_DIR}" "${BASELINE_BACKUP_DIR}"
        fi
        if mv "${BASELINE_STAGE_DIR}" "${BASELINE_DIR}"; then
            rm -rf "${BASELINE_BACKUP_DIR}"
            echo "BASELINE: saved to ${BASELINE_DIR}"
        else
            echo "BASELINE: failed to install staged baseline" >&2
            rc=1
            if [[ -e "${BASELINE_BACKUP_DIR}" ]]; then
                mv "${BASELINE_BACKUP_DIR}" "${BASELINE_DIR}"
            fi
        fi
    fi
    echo "======================================================="
fi

if [[ "${rc:-0}" -ne 0 ]]; then
    exit "${rc}"
fi


