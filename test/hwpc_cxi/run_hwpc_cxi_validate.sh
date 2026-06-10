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

# Run a minimal MPI send/recv workload for HWPC_CXI validation and capture:
#   1) test stdout
#   2) test stderr
#   3) HWPC_CXI report files matching OMPI_MCA_mpi_hwpc_cxi_counter_report_file*
#
# Usage: run_hwpc_cxi_validate.sh [--save-baseline] [num_procs [num_ppn [loops]]]
#   --save-baseline  Save this run's outputs as the reference baseline for future
#                    comparisons.  Baseline stored in: validation_logs/baseline/
#                    Without this flag, any existing baseline is compared automatically.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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
export OMPI_MCA_mpi_hwpc_cxi_counter_file="${OMPI_MCA_mpi_hwpc_cxi_counter_file:-${SCRIPT_DIR}/my_desired_cxi_counters.txt}"
export OMPI_MCA_mpi_hwpc_cxi_counter_report="${OMPI_MCA_mpi_hwpc_cxi_counter_report:-2}"
export OMPI_MCA_mpi_hwpc_cxi_counter_report_filter_zeros="${OMPI_MCA_mpi_hwpc_cxi_counter_report_filter_zeros:-false}"
export OMPI_MCA_mpi_hwpc_cxi_counter_verbose="${OMPI_MCA_mpi_hwpc_cxi_counter_verbose:-true}"
export OMPI_MCA_mpi_hwpc_cxi_counter_report_file="${OMPI_MCA_mpi_hwpc_cxi_counter_report_file:-test_report_prefix}"
#export OMPI_MCA_btl="${OMPI_MCA_btl:-^ofi}"

export HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH:-__REPLACE_ME_RUNTIME_LD_LIBRARY_PATH__}"
if [[ "${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH}" == __REPLACE_ME_* ]]; then
	echo "ERROR: placeholder path detected. Set HWPC_CXI_RUNTIME_LD_LIBRARY_PATH to required runtime library directories." >&2
	echo "  HWPC_CXI_RUNTIME_LD_LIBRARY_PATH=${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH}" >&2
	exit 1
fi
export LD_LIBRARY_PATH="${HWPC_CXI_RUNTIME_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH:-}"

TEST_BIN="${SCRIPT_DIR}/hwpc_cxi_sendrecv_test"
STDOUT_LOG="${RUN_DIR}/stdout.log"
STDERR_LOG="${RUN_DIR}/stderr.log"

if [[ ! -x "${TEST_BIN}" ]]; then
	echo "ERROR: ${TEST_BIN} is missing or not executable. Build tests first with 'make -C test/hwpc_cxi'." >&2
	exit 1
fi

REPORT_PREFIX="${OMPI_MCA_mpi_hwpc_cxi_counter_report_file}"
REPORT_DIR="$(dirname "${REPORT_PREFIX}")"
REPORT_BASE="$(basename "${REPORT_PREFIX}")"
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
		function is_counter_line() {
			return (NF >= 8 && $1 != "Counter" && is_num($2) && is_num($3))
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
			# Activate once "PE 0:" is seen (only when the file has such lines).
			if (base_has_pe0 && $0 ~ /^PE 0:/) {
				base_active = 1
			}
			# Before the activation point: skip everything except error lines.
			if (base_has_pe0 && !base_active) {
				if (tolower($0) ~ /error/) {
					print "ERROR baseline pre-activation line: " $0
					err = 1
				}
				next
			}
			if (!is_counter_line()) {
				next
			}
			name = $1
			if (!(name in seen_base)) {
				seen_base[name] = 1
				base_names[++n_base] = name
			}
			base_count[name] = NF - 1
			for (i = 2; i <= NF; ++i) {
				base_val[name, i - 1] = $i
			}
			next
		}
		{
			# Activate once "PE 0:" is seen (only when the file has such lines).
			if (curr_has_pe0 && $0 ~ /^PE 0:/) {
				curr_active = 1
			}
			# Before the activation point: skip everything except error lines.
			if (curr_has_pe0 && !curr_active) {
				if (tolower($0) ~ /error/) {
					print "ERROR current pre-activation line: " $0
					err = 1
				}
				next
			}
			if (!is_counter_line()) {
				next
			}
			name = $1
			if (!(name in seen_curr)) {
				seen_curr[name] = 1
				curr_names[++n_curr] = name
			}
			curr_count[name] = NF - 1
			for (i = 2; i <= NF; ++i) {
				curr_val[name, i - 1] = $i
			}
		}
		END {
			print "Counter comparison (tolerance=" tolerance_pct "%)"
			for (idx = 1; idx <= n_base; ++idx) {
				name = base_names[idx]
				if (!(name in seen_curr)) {
					print "ERROR missing counter in current log: " name
					err = 1
					continue
				}

				if (base_count[name] != curr_count[name]) {
					print "ERROR counter column mismatch for " name ": baseline has " base_count[name] ", current has " curr_count[name]
					err = 1
					continue
				}

				for (i = 1; i <= base_count[name]; ++i) {
					b = base_val[name, i]
					c = curr_val[name, i]
					if (!is_num(b) || !is_num(c)) {
						print "ERROR non-numeric field for counter " name " column " i ": baseline='" b "' current='" c "'"
						err = 1
						continue
					}

					if ((b + 0) == 0) {
						pct = ((c + 0) == 0) ? 0 : 100
					} else {
						pct = (abs((c + 0) - (b + 0)) / abs(b + 0)) * 100
					}

					if (pct > tolerance_pct) {
						warn++
						printf("WARN %s col%d baseline=%s current=%s delta=%.2f%%\n", name, i, b, c, pct)
					}
				}
			}

			for (idx = 1; idx <= n_curr; ++idx) {
				name = curr_names[idx]
				if (!(name in seen_base)) {
					print "ERROR new counter in current log (not in baseline): " name
					err = 1
				}
			}

			print "Summary: warnings=" warn ", errors=" err
			exit err
		}
	' "${baseline_log}" "${current_log}" > "${output_log}"
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
if srun --mpi=pmix -n "${NUM_PROCS}" -N "${NUM_PPN}" "${TEST_BIN}" "${LOOPS}" > "${STDOUT_LOG}" 2> "${STDERR_LOG}"; then
	echo "RESULT: PASS"
else
	rc=$?
	echo "RESULT: FAIL (exit code ${rc})"
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
	rm -rf "${BASELINE_DIR}"
	mkdir -p "${BASELINE_DIR}"
	cp "${STDOUT_LOG}" "${BASELINE_DIR}/stdout.log"
	cp "${STDERR_LOG}" "${BASELINE_DIR}/stderr.log"
	if [[ -d "${RUN_DIR}/reports" ]]; then
		mkdir -p "${BASELINE_DIR}/reports"
		cp -f "${RUN_DIR}/reports/"* "${BASELINE_DIR}/reports/" 2>/dev/null || true
	fi
	echo "======================================================="
	echo "BASELINE: saved to ${BASELINE_DIR}"
	echo "======================================================="
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
	fi

	# stderr: apply PE 0: gating so pre-activation preamble differences are
	# ignored; only error-containing lines before PE 0: are flagged.
	if compare_counter_logs "${BASELINE_DIR}/stderr.log" "${STDERR_LOG}" "${RUN_DIR}/stderr_counter_compare.txt" 20 1; then
		echo "stderr:          MATCH (see ${RUN_DIR}/stderr_counter_compare.txt)"
	else
		echo "stderr:          ERROR (missing/invalid counters) -> ${RUN_DIR}/stderr_counter_compare.txt"
		_compare_pass=false
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
				fi
			else
				echo "report ${_rname}: MISSING in current run"
				_compare_pass=false
			fi
		done
		if [[ -d "${RUN_DIR}/reports" ]]; then
			for _crpt in "${RUN_DIR}/reports/"*; do
				_rname="$(basename "${_crpt}")"
				if [[ ! -f "${BASELINE_DIR}/reports/${_rname}" ]]; then
					echo "report ${_rname}: NEW (not in baseline)"
					_compare_pass=false
				fi
			done
		fi
		shopt -u nullglob
	fi

	if $_compare_pass; then
		echo "COMPARISON:      PASS"
	else
		echo "COMPARISON:      FAIL"
	fi
	echo "======================================================="
else
	echo "(no baseline found; run with --save-baseline to create one)"
fi
# ─────────────────────────────────────────────────────────────────────────────

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

# Clean any leftover report files for this prefix.
find "${FAKE_RUN_DIR}" -maxdepth 1 -type f -name "hwpc_cxi_fake_report*" -delete 2>/dev/null || true

_fake_rc=0
OMPI_MCA_mpi_hwpc_cxi_counter_file="${FAKE_COUNTER_FILE}" \
OMPI_MCA_mpi_hwpc_cxi_counter_report_file="${FAKE_REPORT_PREFIX}" \
srun --mpi=pmix -n "${NUM_PROCS}" -N "${NUM_PPN}" "${TEST_BIN}" "${LOOPS}" \
	> "${FAKE_STDOUT_LOG}" 2> "${FAKE_STDERR_LOG}" || _fake_rc=$?

echo "fake_stdout_log: ${FAKE_STDOUT_LOG}"
echo "fake_stderr_log: ${FAKE_STDERR_LOG}"

# The test passes when stderr contains at least one line indicating that a
# counter name was not recognised / does not exist. hwpc_cxi is expected to
# emit messages matching patterns like "does not exist", "not found",
# "unrecognized", "unknown", or "invalid" for the bogus names.
_fake_diag_count=$(grep -ciE \
	'does not exist|not found|not recognized|unrecognized|unknown counter|invalid counter' \
	"${FAKE_STDERR_LOG}" 2>/dev/null || true)

if (( _fake_diag_count > 0 )); then
	echo "FAKE-COUNTER:    PASS (${_fake_diag_count} diagnostic line(s) found in stderr)"
else
	echo "FAKE-COUNTER:    FAIL (no 'counter not found' diagnostics in stderr)"
	echo "  Expected hwpc_cxi to report unrecognised counter names."
	echo "  Inspect: ${FAKE_STDERR_LOG}"
	rc=1
fi
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────

# ── Report-level=1 timeout-summary test ──────────────────────────────────────
# Run with the normal desired counter file, but force report level 1.
# Expected stdout behavior: no CXI counter summary table; only timeout summary
# should be emitted by HWPC_CXI.
REPORT1_COUNTER_FILE="${SCRIPT_DIR}/my_desired_cxi_counters.txt"
echo "========================================================"
echo "REPORT=1 test (timeout summary only)"
echo "  counter_file: ${REPORT1_COUNTER_FILE}"
echo "========================================================"

REPORT1_RUN_DIR="${LOGS_DIR}/report1_${RUN_TAG}"
mkdir -p "${REPORT1_RUN_DIR}"
REPORT1_STDOUT_LOG="${REPORT1_RUN_DIR}/stdout.log"
REPORT1_STDERR_LOG="${REPORT1_RUN_DIR}/stderr.log"
REPORT1_REPORT_PREFIX="${REPORT1_RUN_DIR}/hwpc_cxi_report1"

# Clean any leftover report files for this prefix.
find "${REPORT1_RUN_DIR}" -maxdepth 1 -type f -name "hwpc_cxi_report1*" -delete 2>/dev/null || true

_report1_rc=0
OMPI_MCA_mpi_hwpc_cxi_counter_file="${REPORT1_COUNTER_FILE}" \
OMPI_MCA_mpi_hwpc_cxi_counter_report="1" \
OMPI_MCA_mpi_hwpc_cxi_counter_report_file="${REPORT1_REPORT_PREFIX}" \
srun --mpi=pmix -n "${NUM_PROCS}" -N "${NUM_PPN}" "${TEST_BIN}" "${LOOPS}" \
	> "${REPORT1_STDOUT_LOG}" 2> "${REPORT1_STDERR_LOG}" || _report1_rc=$?

echo "report1_stdout_log: ${REPORT1_STDOUT_LOG}"
echo "report1_stderr_log: ${REPORT1_STDERR_LOG}"

if [[ "${_report1_rc}" -ne 0 ]]; then
	echo "REPORT=1:        FAIL (exit code ${_report1_rc})"
	rc=1
else
	# REPORT=1 must not emit the detailed CXI counter summary table/header.
	if grep -q "OpenMPI Slingshot CXI Counter Summary:" "${REPORT1_STDOUT_LOG}"; then
		echo "REPORT=1:        FAIL (stdout contains CXI Counter Summary table)"
		echo "  Inspect: ${REPORT1_STDOUT_LOG}"
		rc=1
	else
		echo "REPORT=1 stdout: PASS (no detailed counter summary table)"
	fi

	# Compare stderr against baseline stderr when available.
	if [[ -f "${BASELINE_DIR}/stderr.log" ]]; then
		if compare_counter_logs "${BASELINE_DIR}/stderr.log" "${REPORT1_STDERR_LOG}" "${REPORT1_RUN_DIR}/stderr_vs_baseline.txt" 20 1; then
			echo "REPORT=1 stderr: PASS (see ${REPORT1_RUN_DIR}/stderr_vs_baseline.txt)"
		else
			echo "REPORT=1 stderr: FAIL (see ${REPORT1_RUN_DIR}/stderr_vs_baseline.txt)"
			rc=1
		fi
	else
		echo "REPORT=1 stderr: SKIP (no baseline stderr at ${BASELINE_DIR}/stderr.log)"
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
echo "REPORT=0 test (HWPC_CXI fully disabled)"
echo "  counter_file: ${REPORT0_COUNTER_FILE}"
echo "========================================================"

REPORT0_RUN_DIR="${LOGS_DIR}/report0_${RUN_TAG}"
mkdir -p "${REPORT0_RUN_DIR}"
REPORT0_STDOUT_LOG="${REPORT0_RUN_DIR}/stdout.log"
REPORT0_STDERR_LOG="${REPORT0_RUN_DIR}/stderr.log"
REPORT0_REPORT_PREFIX="${REPORT0_RUN_DIR}/hwpc_cxi_report0"

# Clean any leftover report files for this prefix.
find "${REPORT0_RUN_DIR}" -maxdepth 1 -type f -name "hwpc_cxi_report0*" -delete 2>/dev/null || true

_report0_rc=0
OMPI_MCA_mpi_hwpc_cxi_counter_file="${REPORT0_COUNTER_FILE}" \
OMPI_MCA_mpi_hwpc_cxi_counter_report="0" \
OMPI_MCA_mpi_hwpc_cxi_counter_report_file="${REPORT0_REPORT_PREFIX}" \
srun --mpi=pmix -n "${NUM_PROCS}" -N "${NUM_PPN}" "${TEST_BIN}" "${LOOPS}" \
	> "${REPORT0_STDOUT_LOG}" 2> "${REPORT0_STDERR_LOG}" || _report0_rc=$?

echo "report0_stdout_log: ${REPORT0_STDOUT_LOG}"
echo "report0_stderr_log: ${REPORT0_STDERR_LOG}"

if [[ "${_report0_rc}" -ne 0 ]]; then
	echo "REPORT=0:        FAIL (exit code ${_report0_rc})"
	rc=1
else
	# For REPORT=0, stdout may be empty or contain only the sendrecv completion
	# line. No HWPC summary/counter output should appear.
	if grep -qE 'HWPC_CXI|OpenMPI Slingshot' "${REPORT0_STDOUT_LOG}"; then
		echo "REPORT=0 stdout: FAIL (found HWPC output)"
		echo "  Inspect: ${REPORT0_STDOUT_LOG}"
		rc=1
	else
		mapfile -t _report0_stdout_lines < <(grep -v '^[[:space:]]*$' "${REPORT0_STDOUT_LOG}" || true)
		if (( ${#_report0_stdout_lines[@]} == 0 )); then
			echo "REPORT=0 stdout: PASS (empty)"
		elif (( ${#_report0_stdout_lines[@]} == 1 )) && [[ "${_report0_stdout_lines[0]}" == "hwpc_cxi_sendrecv_test complete" ]]; then
			echo "REPORT=0 stdout: PASS (single completion line)"
		else
			echo "REPORT=0 stdout: FAIL (unexpected stdout content)"
			echo "  Inspect: ${REPORT0_STDOUT_LOG}"
			rc=1
		fi
	fi

	if grep -qiE 'HWPC_CXI|OpenMPI Slingshot' "${REPORT0_STDERR_LOG}"; then
		echo "REPORT=0 stderr: FAIL (found HWPC output)"
		echo "  Inspect: ${REPORT0_STDERR_LOG}"
		rc=1
	else
		echo "REPORT=0 stderr: PASS (no HWPC output)"
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
echo "REPORT=5 test (per-node report files)"
echo "  counter_file: ${REPORT5_COUNTER_FILE}"
echo "========================================================"

REPORT5_RUN_DIR="${LOGS_DIR}/report5_${RUN_TAG}"
mkdir -p "${REPORT5_RUN_DIR}"
REPORT5_STDOUT_LOG="${REPORT5_RUN_DIR}/stdout.log"
REPORT5_STDERR_LOG="${REPORT5_RUN_DIR}/stderr.log"
REPORT5_REPORT_PREFIX="${REPORT5_RUN_DIR}/hwpc_cxi_report5"
REPORT5_EXPECTED_FILES="${NUM_PPN}"

# Clean any leftover report files for this prefix.
find "${REPORT5_RUN_DIR}" -maxdepth 1 -type f -name "hwpc_cxi_report5*" -delete 2>/dev/null || true

_report5_rc=0
OMPI_MCA_mpi_hwpc_cxi_counter_file="${REPORT5_COUNTER_FILE}" \
OMPI_MCA_mpi_hwpc_cxi_counter_report="5" \
OMPI_MCA_mpi_hwpc_cxi_counter_report_file="${REPORT5_REPORT_PREFIX}" \
srun --mpi=pmix -n "${NUM_PROCS}" -N "${NUM_PPN}" "${TEST_BIN}" "${LOOPS}" \
	> "${REPORT5_STDOUT_LOG}" 2> "${REPORT5_STDERR_LOG}" || _report5_rc=$?

echo "report5_stdout_log: ${REPORT5_STDOUT_LOG}"
echo "report5_stderr_log: ${REPORT5_STDERR_LOG}"

shopt -s nullglob
_report5_files=("${REPORT5_REPORT_PREFIX}".*)
shopt -u nullglob

printf '%s\n' "${_report5_files[@]}" > "${REPORT5_RUN_DIR}/report_files.txt"
echo "report5_file_index: ${REPORT5_RUN_DIR}/report_files.txt"

if (( ${#_report5_files[@]} == 0 )); then
	echo "REPORT=5 files:  FAIL (no files generated for prefix ${REPORT5_REPORT_PREFIX})"
	echo "  Inspect: ${REPORT5_RUN_DIR}/report_files.txt"
	rc=1
else
	echo "REPORT=5 files:  PASS (${#_report5_files[@]} file(s) generated)"
	if [[ "${_report5_rc}" -ne 0 ]]; then
		echo "REPORT=5 note:   srun exited with ${_report5_rc}, but files were generated"
	fi
	if (( ${#_report5_files[@]} != REPORT5_EXPECTED_FILES )); then
		echo "REPORT=5 note:   expected ${REPORT5_EXPECTED_FILES} file(s), found ${#_report5_files[@]}"
	fi
fi
echo "======================================================="
# ─────────────────────────────────────────────────────────────────────────────

if [[ "${rc:-0}" -ne 0 ]]; then
	exit "${rc}"
fi


