#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Fast ``make check`` checks and self-checks that run without an
installed Open MPI."""

import os
from pathlib import Path
import re
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    ABI_CONVERTER_HANDLES, CLASS_IMPLEMENTED, FORTRAN_ABI_HELPERS,
    MPI_REMOVED_LEGACY_C_NAMES, SKIP_CROSS_PROBES_NOT_PASSED,
    SKIP_FORTRAN_BINDINGS_DISABLED, SKIP_FORTRAN_HELPERS_SHARED,
    SKIP_FORTRAN_WRAPPER_UNAVAILABLE, SKIP_HEADER_UNAVAILABLE,
    SKIP_LINKAGE_INSPECTION_UNAVAILABLE, TEST_NOT_WRITTEN,
    VALID_CLASSIFICATIONS, VALID_TEST_STATUSES, _append_check,
    _extend_checks, _fail, _first_existing_build_source_path,
    _missing_build_source_path, _pass, _read_text, _skip)
from _abi_tables import (
    INSTALLED_FORTRAN_COMPILE_PROBES, INSTALLED_FORTRAN_RUNTIME_PROBES)
from _abi_discovery import (
    _abi_suitability, _all_named_executables, _mpich_identity,
    _mpich_report_candidates, _open_mpi_identity,
    _open_mpi_report_candidates, _same_tool_directory, _words_define_macro,
    _words_link_library)
from _abi_probes import (
    _callback_api_coverage_audit, _metadata_integer_value,
    _parse_c_header_prototypes, _parse_header_constants)
from _abi_installed import (
    _c_parameter_normalization_unit_checks, _cross_header_feature_set_check,
    _cross_runtime_library_target, _darwin_otool_install_name,
    _fortran_coverage_audit, _fortran_probe_source, _linux_resolved_library,
    _mpich_transport_runtime_env, _non_abi_absence_check,
    _path_is_run_side_abi_library, _signature_comparison_check)
from _abi_cross import (
    _cross_direction_result, _cross_direction_selection,
    _cross_summary_result)


def _manifest_sanity_checks(manifest):
    """Validate internal manifest classifications before deeper checks run."""
    checks = []
    apis = manifest["apis"]
    constants = manifest["constants"]
    for kind, entries in (("api", apis), ("constant", constants)):
        invalid_classifications = [
            entry["name"] for entry in entries
            if entry["classification"] not in VALID_CLASSIFICATIONS
        ]
        if invalid_classifications:
            checks.append(_fail(
                kind + "_classification_values",
                "invalid manifest classification values",
                entries=invalid_classifications[:20],
                count=len(invalid_classifications)))
        else:
            checks.append(_pass(
                kind + "_classification_values", count=len(entries)))

        invalid_statuses = [
            entry["name"] for entry in entries
            if entry["test_status"] not in VALID_TEST_STATUSES
        ]
        if invalid_statuses:
            checks.append(_fail(
                kind + "_test_status_values",
                "invalid manifest test status values",
                entries=invalid_statuses[:20],
                count=len(invalid_statuses)))
        else:
            checks.append(_pass(kind + "_test_status_values",
                                count=len(entries)))

    bad_skip_reasons = [
        entry["name"] for entry in apis + constants
        if ((entry["classification"] == CLASS_IMPLEMENTED
             and entry["skip_reason"] is not None)
            or (entry["classification"] != CLASS_IMPLEMENTED
                and entry["skip_reason"] is None))
    ]
    if bad_skip_reasons:
        checks.append(_fail(
            "manifest_skip_reasons",
            "manifest entries have inconsistent skip reasons",
            entries=bad_skip_reasons[:20],
            count=len(bad_skip_reasons)))
    else:
        checks.append(_pass("manifest_skip_reasons",
                            count=len(apis) + len(constants)))
    return checks


def _standard_abi_header_path(srcdir, builddir):
    """Return the generated source-tree/build-tree standard ABI mpi.h."""
    candidates = (
        builddir / "ompi" / "mpi" / "c" / "standard_abi" / "mpi.h",
        srcdir / "ompi" / "mpi" / "c" / "standard_abi" / "mpi.h",
    )
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return None


def _header_constant_parser_unit_checks():
    """Validate the standard ABI header constant parser.

    The semantic header check depends on parser behavior that is easy to
    erode accidentally: aliases must resolve after all numeric values are
    known, implicit enum values must advance from explicit enum values,
    and unparseable declarations must remain visible as diagnostics
    instead of silently disappearing from coverage.
    """
    check_name = "fast_header_constant_parser_unit_checks"
    header_text = """
#define MPI_ALIAS_TARGET 11
#define MPI_ALIAS MPI_ALIAS_TARGET
#define MPI_SHIFTED (1 << 4)
#define MPI_UNRESOLVED_ALIAS MPI_UNKNOWN_TARGET
enum {
    MPI_ENUM_START = 30,
    MPI_ENUM_NEXT,
};
enum {
    MPI_SECOND_ENUM_START = 7,
    MPI_SECOND_ENUM_NEXT,
};
"""
    with tempfile.TemporaryDirectory() as tmpdir:
        path = Path(tmpdir) / "mpi.h"
        path.write_text(header_text)
        constants, unparsed = _parse_header_constants(path)
    expected = {
        "MPI_ALIAS_TARGET": 11,
        "MPI_ALIAS": 11,
        "MPI_SHIFTED": 16,
        "MPI_ENUM_START": 30,
        "MPI_ENUM_NEXT": 31,
        "MPI_SECOND_ENUM_START": 7,
        "MPI_SECOND_ENUM_NEXT": 8,
    }
    mismatches = {
        name: constants.get(name)
        for name, value in expected.items()
        if constants.get(name) != value
    }
    if mismatches:
        return _fail(
            check_name,
            "Header constant parser did not preserve numeric values",
            check="numeric_values",
            mismatches=mismatches,
            constants=constants)
    if "MPI_UNRESOLVED_ALIAS" not in unparsed:
        return _fail(
            check_name,
            "Header constant parser dropped an unparseable declaration",
            check="unparsed_diagnostics",
            unparsed=unparsed)
    return _pass(check_name)


def _legacy_tolerance_unit_checks():
    """Validate MPICH-only tolerance for removed MPI-1 C names.

    MPICH currently exposes a few names that MPI-5 removed from the
    standard C binding surface.  The cross runner tolerates those names
    only as extra MPICH declarations so the compatibility report can
    focus on the MPI Forum ABI surface.  The asymmetry is important:
    Open MPI exposing the same removed names, or either implementation
    missing a non-legacy standard name, must still fail.
    """
    check_name = "fast_legacy_tolerance_unit_checks"
    expected_signatures = {
        "MPI_Init": {
            "name": "MPI_Init",
            "return_type": "int",
            "args": "void",
            "signature": "int MPI_Init(void)",
        },
        "MPI_Attr_put": {
            "name": "MPI_Attr_put",
            "return_type": "int",
            "args": "MPI_Comm comm, int keyval, void *attribute_val",
            "signature": (
                "int MPI_Attr_put(MPI_Comm comm, int keyval, "
                "void *attribute_val)"
            ),
        },
    }
    open_mpi_header_text = """
int MPI_Init(void);
"""
    mpich_header_text = """
#define MPI_DUP_FN 1
int MPI_Init(void);
int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val);
"""
    reverse_open_mpi_header_text = mpich_header_text
    reverse_mpich_header_text = open_mpi_header_text
    manifest = {
        "apis": [
            {
                "name": "MPI_Attr_put",
                "classification": CLASS_IMPLEMENTED,
                "callback": True,
                "languages": {"c": True},
            },
        ],
        "constants": [
            {
                "name": "MPI_DUP_FN",
                "c_type": "int",
            },
        ],
    }

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)
        open_mpi_header = tmp / "open_mpi_mpi.h"
        mpich_header = tmp / "mpich_mpi.h"
        reverse_open_mpi_header = tmp / "reverse_open_mpi_mpi.h"
        reverse_mpich_header = tmp / "reverse_mpich_mpi.h"
        open_mpi_header.write_text(open_mpi_header_text)
        mpich_header.write_text(mpich_header_text)
        reverse_open_mpi_header.write_text(reverse_open_mpi_header_text)
        reverse_mpich_header.write_text(reverse_mpich_header_text)

        prototypes = _parse_c_header_prototypes(mpich_header)
        signature = _signature_comparison_check(
            prototypes,
            {"MPI_Init": expected_signatures["MPI_Init"]},
            check_name=check_name + "_signature",
            tolerated_extra_names=MPI_REMOVED_LEGACY_C_NAMES)
        if signature["result"] != "PASS":
            return _fail(
                check_name,
                "Removed legacy names must be tolerated as MPICH-only "
                "extra prototypes",
                check="signature_tolerates_mpich_legacy",
                result=signature)

        signature = _signature_comparison_check(
            prototypes,
            {"MPI_Init": expected_signatures["MPI_Init"]},
            check_name=check_name + "_signature_no_tolerance")
        if signature["result"] != "FAIL":
            return _fail(
                check_name,
                "Removed legacy names must still fail without an "
                "explicit tolerance set",
                check="signature_fails_without_tolerance",
                result=signature)

        absence = _non_abi_absence_check(
            prototypes,
            {"MPI_Attr_put"},
            check_name=check_name + "_absence",
            tolerated_exposed_names=MPI_REMOVED_LEGACY_C_NAMES)
        if absence["result"] != "PASS":
            return _fail(
                check_name,
                "Removed legacy names must be tolerated as MPICH-only "
                "excluded API declarations",
                check="absence_tolerates_mpich_legacy",
                result=absence)

        absence = _non_abi_absence_check(
            prototypes,
            {"MPI_Attr_put"},
            check_name=check_name + "_absence_no_tolerance")
        if absence["result"] != "FAIL":
            return _fail(
                check_name,
                "Removed legacy names must still fail the non-ABI "
                "absence check without tolerance",
                check="absence_fails_without_tolerance",
                result=absence)

        callback = _callback_api_coverage_audit(
            manifest,
            mpich_header,
            [],
            check_name=check_name + "_callback",
            tolerated_legacy_names=MPI_REMOVED_LEGACY_C_NAMES)
        if callback["result"] != "PASS":
            return _fail(
                check_name,
                "Removed legacy callback APIs must be tolerated as "
                "MPICH-only declarations",
                check="callback_tolerates_mpich_legacy",
                result=callback)
        callback_details = callback["details"]
        if (callback_details.get("tolerated_legacy_declared") !=
                ["MPI_Attr_put"] or
                callback_details.get(
                    "tolerated_legacy_declared_count") != 1 or
                callback_details.get("missing_count") != 0):
            return _fail(
                check_name,
                "Removed legacy callback tolerance details must remain "
                "machine-readable",
                check="callback_tolerance_details",
                result=callback)

        callback = _callback_api_coverage_audit(
            manifest,
            mpich_header,
            [],
            check_name=check_name + "_callback_no_tolerance")
        if callback["result"] != "FAIL":
            return _fail(
                check_name,
                "Removed legacy callback APIs must still fail the "
                "callback audit without tolerance",
                check="callback_fails_without_tolerance",
                result=callback)

        feature = _cross_header_feature_set_check(
            {"open_mpi": open_mpi_header, "mpich": mpich_header},
            manifest,
            expected_signatures)
        if (feature["result"] != "PASS" or
                feature["details"].get("tolerated_api_only_mpich_count") !=
                1 or
                feature["details"].get(
                    "tolerated_constants_only_mpich_count") != 1):
            return _fail(
                check_name,
                "MPICH-only removed legacy declarations must be "
                "partitioned as tolerated cross-header extras",
                check="cross_header_tolerates_mpich_legacy",
                result=feature)

        feature = _cross_header_feature_set_check(
            {
                "open_mpi": reverse_open_mpi_header,
                "mpich": reverse_mpich_header,
            },
            manifest,
            expected_signatures)
        if feature["result"] != "FAIL":
            return _fail(
                check_name,
                "Open-MPI-only removed legacy declarations must not be "
                "tolerated by cross-header comparison",
                check="cross_header_rejects_open_mpi_legacy",
                result=feature)

    return _pass(check_name)


def _all_report_checks(report):
    """Return every concrete check result embedded in a report."""
    checks = []
    for key in ("fast_checks", "installed_checks", "cross_checks"):
        checks.extend(report.get(key, ()))
    return checks


def _checks_named(report, name):
    """Return all report checks with a given stable name."""
    return [
        check for check in _all_report_checks(report)
        if check.get("name") == name
    ]


def _completion_gate_add_finding(findings, kind, message, **details):
    """Append one stable completion-gate finding."""
    finding = {
        "kind": kind,
        "message": message,
    }
    finding.update(details)
    findings.append(finding)


def _completion_gate_report(manifest, report):
    """Build the completed-suite gate result for one finished report.

    The normal report contains the evidence; this helper only decides
    whether that evidence is strong enough to declare the suite complete.
    A legitimate mode-level SKIP remains a PASS for the gate because the
    spec explicitly allows, for example, standard-ABI-disabled builds to
    skip rather than fail.  Any requested mode that actually ran and
    exposed missing implemented coverage is a hard completion failure.
    """
    findings = []
    if report["result"] == "SKIP":
        return {
            "result": "PASS",
            "skip_preserved": True,
            "findings": [],
            "finding_count": 0,
        }

    entries = list(manifest["apis"]) + list(manifest["constants"])
    invalid_classifications = [
        entry["name"] for entry in entries
        if entry["classification"] not in VALID_CLASSIFICATIONS
    ]
    if invalid_classifications:
        _completion_gate_add_finding(
            findings,
            "unclassified_metadata",
            "metadata entries have invalid classifications",
            count=len(invalid_classifications),
            entries=invalid_classifications[:20])

    invalid_test_status = [
        entry["name"] for entry in entries
        if entry["test_status"] not in VALID_TEST_STATUSES
    ]
    if invalid_test_status:
        _completion_gate_add_finding(
            findings,
            "invalid_test_status",
            "metadata entries have invalid test status values",
            count=len(invalid_test_status),
            entries=invalid_test_status[:20])

    api_not_written = [
        entry["name"] for entry in manifest["apis"]
        if (entry["classification"] == CLASS_IMPLEMENTED and
            entry["test_status"] == TEST_NOT_WRITTEN)
    ]
    if api_not_written:
        _completion_gate_add_finding(
            findings,
            "api_test_not_written_yet",
            "implemented ABI APIs still have test_not_written_yet status",
            count=len(api_not_written),
            entries=api_not_written[:20])

    constant_not_written = [
        entry["name"] for entry in manifest["constants"]
        if (entry["classification"] == CLASS_IMPLEMENTED and
            entry["test_status"] == TEST_NOT_WRITTEN)
    ]
    if constant_not_written:
        _completion_gate_add_finding(
            findings,
            "constant_test_not_written_yet",
            "implemented ABI constants still have test_not_written_yet "
            "status",
            count=len(constant_not_written),
            entries=constant_not_written[:20])

    failed_checks = [
        check for check in _all_report_checks(report)
        if check.get("result") == "FAIL"
    ]
    if failed_checks:
        _completion_gate_add_finding(
            findings,
            "failed_checks",
            "report contains failed checks",
            count=len(failed_checks),
            checks=[check["name"] for check in failed_checks[:20]])

    mode = report["mode"]
    if mode == "check-abi":
        runtime_audits = _checks_named(
            report, "installed_c_runtime_api_coverage_audit")
        callback_audits = _checks_named(
            report, "installed_c_callback_api_coverage_audit")
        for audit_name, audits in (
                ("installed_c_runtime_api_coverage_audit", runtime_audits),
                ("installed_c_callback_api_coverage_audit", callback_audits)):
            if not audits:
                _completion_gate_add_finding(
                    findings,
                    "missing_completion_audit",
                    "required installed C coverage audit did not run",
                    audit=audit_name)
                continue
            for audit in audits:
                details = audit.get("details", {})
                missing = details.get("missing_count", 0)
                unclassified = details.get(
                    "unclassified_callback_api_count", 0)
                if missing or unclassified:
                    _completion_gate_add_finding(
                        findings,
                        "missing_c_coverage",
                        "installed C coverage audit has undeferred gaps",
                        audit=audit_name,
                        missing_count=missing,
                        unclassified_callback_api_count=unclassified,
                        missing_by_package=details.get(
                            "missing_by_package", {}))

        fortran_audits = _checks_named(
            report, "installed_fortran_coverage_audit")
        if not fortran_audits:
            _completion_gate_add_finding(
                findings,
                "missing_completion_audit",
                "required Fortran coverage audit did not run",
                audit="installed_fortran_coverage_audit")
        for audit in fortran_audits:
            languages = audit.get("details", {}).get("languages", {})
            pending = {}
            for language, info in sorted(languages.items()):
                configured = info.get("configured", {}).get("enabled")
                pending_count = info.get("pending_phase11b_count", 0)
                if configured is not False and pending_count:
                    pending[language] = {
                        "count": pending_count,
                        "sample": info.get("pending_phase11b", []),
                    }
            if pending:
                _completion_gate_add_finding(
                    findings,
                    "missing_fortran_coverage",
                    "configured Fortran binding layers still have "
                    "pending ABI coverage",
                    languages=pending)

    if mode == "check-abi-mpich":
        summaries = _checks_named(report, "cross_direction_summary")
        if not summaries:
            _completion_gate_add_finding(
                findings,
                "missing_completion_audit",
                "MPICH cross-direction summary did not run",
                audit="cross_direction_summary")
        else:
            incomplete_summaries = []
            for summary in summaries:
                details = summary.get("details", {})
                direction_count = len(details.get("directions", ()))
                passed = details.get("direction_status", {}).get("PASS", 0)
                if (summary.get("result") != "PASS" or
                        passed != direction_count):
                    incomplete_summaries.append(summary)
        if summaries and incomplete_summaries:
            _completion_gate_add_finding(
                findings,
                "cross_direction_not_passed",
                "MPICH cross-direction probes did not all pass",
                summaries=[
                    {
                        "result": summary.get("result"),
                        "skip_reason": summary.get("skip_reason"),
                        "details": summary.get("details", {}),
                    }
                    for summary in incomplete_summaries
                ])

    return {
        "result": "FAIL" if findings else "PASS",
        "skip_preserved": False,
        "findings": findings,
        "finding_count": len(findings),
    }


def _completion_gate_unit_checks():
    """Validate completed-suite gate PASS/FAIL/SKIP behavior."""
    check_name = "fast_completion_gate_unit_checks"
    empty_manifest = {"apis": [], "constants": []}
    skipped = _completion_gate_report(
        empty_manifest,
        {
            "mode": "check-abi",
            "result": "SKIP",
            "fast_checks": [],
            "installed_checks": [],
            "cross_checks": [],
        })
    if skipped["result"] != "PASS" or not skipped["skip_preserved"]:
        return _fail(
            check_name,
            "Completion gate must preserve legitimate mode-level SKIPs",
            check="skip_preserved",
            gate=skipped)

    passed = _completion_gate_report(
        empty_manifest,
        {
            "mode": "check-fast",
            "result": "PASS",
            "fast_checks": [],
            "installed_checks": [],
            "cross_checks": [],
        })
    if passed["result"] != "PASS":
        return _fail(
            check_name,
            "Completion gate must pass when no findings exist",
            check="empty_pass",
            gate=passed)

    manifest = {
        "apis": [
            {
                "name": "MPI_Test_missing",
                "classification": CLASS_IMPLEMENTED,
                "test_status": TEST_NOT_WRITTEN,
            }
        ],
        "constants": [
            {
                "name": "MPI_CONST_MISSING",
                "classification": CLASS_IMPLEMENTED,
                "test_status": TEST_NOT_WRITTEN,
            }
        ],
    }
    failed = _completion_gate_report(
        manifest,
        {
            "mode": "check-fast",
            "result": "PASS",
            "fast_checks": [],
            "installed_checks": [],
            "cross_checks": [],
        })
    kinds = {finding["kind"] for finding in failed["findings"]}
    if (failed["result"] != "FAIL" or
            "api_test_not_written_yet" not in kinds or
            "constant_test_not_written_yet" not in kinds):
        return _fail(
            check_name,
            "Completion gate must fail on unwritten API and constant "
            "coverage",
            check="not_written_failure",
            gate=failed)

    failed = _completion_gate_report(
        empty_manifest,
        {
            "mode": "check-fast",
            "result": "FAIL",
            "fast_checks": [_fail("synthetic_failure", "failed")],
            "installed_checks": [],
            "cross_checks": [],
        })
    kinds = {finding["kind"] for finding in failed["findings"]}
    if failed["result"] != "FAIL" or "failed_checks" not in kinds:
        return _fail(
            check_name,
            "Completion gate must fail when report checks fail",
            check="failed_check_failure",
            gate=failed)

    missing_audit = _completion_gate_report(
        empty_manifest,
        {
            "mode": "check-abi",
            "result": "PASS",
            "fast_checks": [],
            "installed_checks": [],
            "cross_checks": [],
        })
    kinds = {finding["kind"] for finding in missing_audit["findings"]}
    if ("missing_completion_audit" not in kinds or
            missing_audit["result"] != "FAIL"):
        return _fail(
            check_name,
            "Completion gate must require installed coverage audits",
            check="missing_installed_audits",
            gate=missing_audit)

    missing_c_coverage = _completion_gate_report(
        empty_manifest,
        {
            "mode": "check-abi",
            "result": "PASS",
            "fast_checks": [],
            "installed_checks": [
                _pass(
                    "installed_c_runtime_api_coverage_audit",
                    missing_count=1,
                    missing_by_package={"phase9_runtime": ["MPI_X"]}),
                _pass(
                    "installed_c_callback_api_coverage_audit",
                    missing_count=0,
                    unclassified_callback_api_count=0),
                _pass(
                    "installed_fortran_coverage_audit",
                    languages={}),
            ],
            "cross_checks": [],
        })
    kinds = {finding["kind"] for finding in missing_c_coverage["findings"]}
    if ("missing_c_coverage" not in kinds or
            missing_c_coverage["result"] != "FAIL"):
        return _fail(
            check_name,
            "Completion gate must fail on installed C audit gaps",
            check="missing_c_coverage",
            gate=missing_c_coverage)

    fortran_pending = _completion_gate_report(
        empty_manifest,
        {
            "mode": "check-abi",
            "result": "PASS",
            "fast_checks": [],
            "installed_checks": [
                _pass(
                    "installed_c_runtime_api_coverage_audit",
                    missing_count=0,
                    unclassified_callback_api_count=0),
                _pass(
                    "installed_c_callback_api_coverage_audit",
                    missing_count=0,
                    unclassified_callback_api_count=0),
                _pass(
                    "installed_fortran_coverage_audit",
                    languages={
                        "mpi_f08": {
                            "configured": {"enabled": None},
                            "pending_phase11b_count": 1,
                            "pending_phase11b": ["MPI_F08_X"],
                        },
                    }),
            ],
            "cross_checks": [],
        })
    kinds = {finding["kind"] for finding in fortran_pending["findings"]}
    if ("missing_fortran_coverage" not in kinds or
            fortran_pending["result"] != "FAIL"):
        return _fail(
            check_name,
            "Completion gate must treat unknown configured Fortran state "
            "as coverage-relevant",
            check="fortran_unknown_pending",
            gate=fortran_pending)

    mixed_cross = _completion_gate_report(
        empty_manifest,
        {
            "mode": "check-abi-mpich",
            "result": "PASS",
            "fast_checks": [],
            "installed_checks": [],
            "cross_checks": [
                _pass(
                    "cross_direction_summary",
                    directions=("open_mpi_compile_mpich_run",
                                "mpich_compile_open_mpi_run"),
                    direction_status={"PASS": 1, "SKIP": 1}),
            ],
        })
    kinds = {finding["kind"] for finding in mixed_cross["findings"]}
    if ("cross_direction_not_passed" not in kinds or
            mixed_cross["result"] != "FAIL"):
        return _fail(
            check_name,
            "Completion gate must fail when any requested cross "
            "direction skips",
            check="mixed_cross_direction",
            gate=mixed_cross)

    return _pass(check_name)


def _header_constant_checks(manifest, srcdir, builddir):
    """Compare generated standard ABI header constants to ABI metadata.

    This is the fast, in-tree constant check.  It validates the generated
    header against docs/ metadata without requiring an installed Open MPI
    or mpirun, so it can run before the installed runtime probes.
    """
    path = _standard_abi_header_path(srcdir, builddir)
    if path is None:
        return [_skip("standard_abi_header_constants",
                      SKIP_HEADER_UNAVAILABLE)]

    header_constants, unparsed_header_constants = _parse_header_constants(path)
    missing = []
    mismatches = []
    unparsed = []
    checked = 0
    skipped = []
    for entry in manifest["constants"]:
        name = entry["name"]
        expected = _metadata_integer_value(entry["abi_value"])
        if expected is None:
            skipped.append(name)
            continue
        if entry["c_type"] is None:
            skipped.append(name)
            continue
        if entry["category"] == "DEPRECATED_FUNCS":
            skipped.append(name)
            continue
        checked += 1
        if name not in header_constants:
            if name in unparsed_header_constants:
                unparsed.append(name)
            else:
                missing.append(name)
        elif header_constants[name] != expected:
            mismatches.append({
                "name": name,
                "expected": expected,
                "actual": header_constants[name],
            })

    if checked == 0:
        return [_fail(
            "standard_abi_header_constants",
            "no standard ABI header constants were validated",
            header=str(path),
            checked=checked,
            skipped_count=len(skipped),
            parsed_header_count=len(header_constants),
            unparsed_header_count=len(unparsed_header_constants))]

    if missing or mismatches or unparsed:
        return [_fail(
            "standard_abi_header_constants",
            "standard ABI header constants differ from metadata",
            header=str(path),
            checked=checked,
            skipped_count=len(skipped),
            missing=missing[:20],
            missing_count=len(missing),
            unparsed=unparsed[:20],
            unparsed_count=len(unparsed),
            mismatches=mismatches[:20],
            mismatch_count=len(mismatches))]

    return [_pass("standard_abi_header_constants",
                  header=str(path),
                  checked=checked,
                  skipped_count=len(skipped))]


def _require_substrings(text, requirements):
    missing = []
    for label, substring in requirements:
        if substring not in text:
            missing.append(label)
    return missing


def _token_pattern(token):
    return r"(?<![A-Za-z0-9_])" + re.escape(token) + \
           r"(?![A-Za-z0-9_])"


def _contains_token(text, token):
    return re.search(_token_pattern(token), text) is not None


def _abi_converter_checks(srcdir, builddir):
    """Check source contracts for ABI converter implementation files.

    These are source-contract checks, not behavioral unit tests.  They
    make sure each converter source is present, wired into the ABI
    makefile, and contains the expected wrapper/conversion structure.
    Runtime converter behavior is exercised later by installed C probes.
    """
    c_relative = Path("ompi") / "mpi" / "c"
    makefile = _first_existing_build_source_path(
        srcdir, builddir, c_relative / "Makefile_abi.include")
    converter_header = _first_existing_build_source_path(
        srcdir, builddir, c_relative / "abi_converters.h")
    converter_source = _first_existing_build_source_path(
        srcdir, builddir, c_relative / "abi_converters.c")
    missing_files = []
    missing_patterns = []

    required_files = (
        (makefile, c_relative / "Makefile_abi.include"),
        (converter_header, c_relative / "abi_converters.h"),
        (converter_source, c_relative / "abi_converters.c"),
    )
    for path, relative_path in required_files:
        if path is None:
            missing_files.append(
                _missing_build_source_path(srcdir, builddir, relative_path))

    if missing_files:
        return [_fail(
            "abi_converter_sources",
            "required ABI converter files are missing",
            missing_files=missing_files)]

    makefile_text = _read_text(makefile)
    header_text = _read_text(converter_header)
    source_text = _read_text(converter_source)

    header_requirements = [
        ("OMPI_ABI_HANDLE_BASE_OFFSET", "#define OMPI_ABI_HANDLE_BASE_OFFSET"),
        ("ABI base offset value", "16385"),
        ("error conversion to OMPI", "ompi_convert_abi_error_intern_error"),
        ("error conversion to ABI", "ompi_convert_intern_error_abi_error"),
        ("status conversion to OMPI", "ompi_convert_abi_status_intern_status"),
        ("status conversion to ABI", "ompi_convert_intern_status_abi_status"),
    ]
    for label in _require_substrings(header_text, header_requirements):
        missing_patterns.append({
            "file": str(converter_header),
            "pattern": label,
        })

    source_requirements = [
        ("errhandler argument converter",
         "ompi_convert_errhandler_args_intern_to_abi"),
        ("communicator callback conversion",
         "ompi_convert_comm_ompi_to_standard"),
        ("window callback conversion", "ompi_convert_win_ompi_to_standard"),
        ("file callback conversion", "ompi_convert_file_ompi_to_standard"),
        ("session callback conversion",
         "ompi_convert_session_ompi_to_standard"),
    ]
    for label in _require_substrings(source_text, source_requirements):
        missing_patterns.append({
            "file": str(converter_source),
            "pattern": label,
        })

    checked_sources = 0
    for symbol, stem in ABI_CONVERTER_HANDLES:
        to_relative = c_relative / (stem + "_toint_abi.c")
        from_relative = c_relative / (stem + "_fromint_abi.c")
        to_file = _first_existing_build_source_path(
            srcdir, builddir, to_relative)
        from_file = _first_existing_build_source_path(
            srcdir, builddir, from_relative)
        to_source_name = to_relative.name
        from_source_name = from_relative.name
        to_symbol = "MPI_" + symbol + "_toint"
        from_symbol = "MPI_" + symbol + "_fromint"
        pto_symbol = "PMPI_" + symbol + "_toint"
        pfrom_symbol = "PMPI_" + symbol + "_fromint"

        for path, relative_path in ((to_file, to_relative),
                                    (from_file, from_relative)):
            if path is None:
                missing_files.append(
                    _missing_build_source_path(srcdir, builddir,
                                               relative_path))

        for source_name in (to_source_name, from_source_name):
            if source_name not in makefile_text:
                missing_patterns.append({
                    "file": str(makefile),
                    "pattern": source_name,
                })

        if to_file is not None:
            to_text = _read_text(to_file)
            to_requirements = [
                ("abi.h include", '#include "ompi/mpi/c/abi.h"'),
                ("abi_converters.h include",
                 '#include "ompi/mpi/c/abi_converters.h"'),
                ("profiling weak symbol",
                 "#pragma weak {0} = {1}".format(to_symbol, pto_symbol)),
                ("function name", 'FUNC_NAME[] = "{0}"'.format(to_symbol)),
                ("public function", to_symbol + "("),
                ("preserve ABI integer handles",
                 "OMPI_ABI_HANDLE_BASE_OFFSET >"),
                ("apply ABI base offset",
                 "+= OMPI_ABI_HANDLE_BASE_OFFSET"),
            ]
            for label in _require_substrings(to_text, to_requirements):
                missing_patterns.append({
                    "file": str(to_file),
                    "pattern": label,
                })
            checked_sources += 1

        if from_file is not None:
            from_text = _read_text(from_file)
            from_requirements = [
                ("abi.h include", '#include "ompi/mpi/c/abi.h"'),
                ("abi_converters.h include",
                 '#include "ompi/mpi/c/abi_converters.h"'),
                ("profiling weak symbol",
                 "#pragma weak {0} = {1}".format(from_symbol, pfrom_symbol)),
                ("function name", 'FUNC_NAME[] = "{0}"'.format(from_symbol)),
                ("public function", from_symbol + "("),
                ("preserve ABI integer handles",
                 "OMPI_ABI_HANDLE_BASE_OFFSET >"),
                ("remove ABI base offset",
                 "- OMPI_ABI_HANDLE_BASE_OFFSET"),
                ("recover OMPI object", "opal_pointer_array_get_item"),
            ]
            for label in _require_substrings(from_text, from_requirements):
                missing_patterns.append({
                    "file": str(from_file),
                    "pattern": label,
                })
            checked_sources += 1

    if missing_files or missing_patterns:
        return [_fail(
            "abi_converter_sources",
            "ABI converter source checks failed",
            checked_sources=checked_sources,
            missing_files=missing_files[:20],
            missing_file_count=len(missing_files),
            missing_patterns=missing_patterns[:20],
            missing_pattern_count=len(missing_patterns))]

    return [_pass("abi_converter_sources",
                  checked_sources=checked_sources,
                  handle_count=len(ABI_CONVERTER_HANDLES))]


def _fortran_enabled(manifest, language):
    """Return whether one Fortran binding layer was configured."""
    return manifest["configuration"]["fortran"][language]["enabled"]


def _fortran_mpifh_helper_checks(srcdir, builddir, manifest):
    """Check mpif.h ABI helper source contracts.

    The helper entry points support legacy Fortran binding layers even
    though the standard ABI is centered on mpi_f08.  This check verifies
    the generated symbol names and PMPI forwarding contracts without
    requiring a Fortran compiler or installed runtime.
    """
    enabled = _fortran_enabled(manifest, "mpif.h")
    if enabled is False:
        return [_skip("fortran_mpifh_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="mpif.h")]

    base_relative = Path("ompi") / "mpi" / "fortran" / "mpif-h"
    makefile = _first_existing_build_source_path(
        srcdir, builddir, base_relative / "Makefile.am")
    prototypes = _first_existing_build_source_path(
        srcdir, builddir, base_relative / "prototypes_mpi.h")
    missing_files = []
    missing_patterns = []

    for path, relative_path in (
            (makefile, base_relative / "Makefile.am"),
            (prototypes, base_relative / "prototypes_mpi.h")):
        if path is None:
            missing_files.append(
                _missing_build_source_path(srcdir, builddir, relative_path))

    makefile_text = _read_text(makefile) if makefile is not None else ""
    prototypes_text = _read_text(prototypes) if prototypes is not None else ""
    checked_sources = 0

    for helper in FORTRAN_ABI_HELPERS:
        c_name = helper + "_f.c"
        c_relative = base_relative / c_name
        c_path = _first_existing_build_source_path(
            srcdir, builddir, c_relative)
        helper_suffix = helper[4:]
        mixed_name = "MPI_Abi_" + helper_suffix
        lower_name = "mpi_abi_" + helper_suffix
        upper_name = lower_name.upper()
        internal_name = "ompi_" + helper + "_f"
        pmpi_name = "PMPI_Abi_" + helper_suffix

        if c_path is None:
            missing_files.append(
                _missing_build_source_path(srcdir, builddir, c_relative))
            continue

        checked_sources += 1
        text = _read_text(c_path)
        requirements = [
            ("Makefile source", c_name),
            ("prototype declaration", mixed_name),
            ("uppercase weak symbol", upper_name),
            ("lowercase weak symbol", lower_name),
            ("mixed-case weak symbol", mixed_name + "_f"),
            ("f08 weak symbol", mixed_name + "_f08"),
            ("internal wrapper", internal_name),
            ("calls PMPI C helper", pmpi_name),
        ]
        for label, pattern in requirements:
            haystack = makefile_text if label == "Makefile source" else text
            if label == "prototype declaration":
                haystack = prototypes_text
            found = pattern in haystack
            if label != "Makefile source":
                found = _contains_token(haystack, pattern)
            if not found:
                missing_patterns.append({
                    "file": str(c_path if label != "prototype declaration"
                                else prototypes),
                    "pattern": label,
                    "helper": helper,
                })

    if missing_files or missing_patterns:
        return [_fail(
            "fortran_mpifh_abi_helpers",
            "mpif.h ABI helper source checks failed",
            configured=enabled,
            checked_sources=checked_sources,
            missing_files=missing_files[:20],
            missing_file_count=len(missing_files),
            missing_patterns=missing_patterns[:20],
            missing_pattern_count=len(missing_patterns))]

    return [_pass("fortran_mpifh_abi_helpers",
                  configured=enabled,
                  checked_sources=checked_sources)]


def _fortran_usempi_helper_checks(srcdir, builddir, manifest):
    """Check the use mpi layer's shared-helper status.

    use mpi shares the mpif.h helper entry points in this code base, so
    there is no independent per-helper implementation to validate here.
    Reporting a stable SKIP is more honest than a PASS with no specific
    layer contract.
    """
    enabled = _fortran_enabled(manifest, "use mpi")
    if enabled is False:
        return [_skip("fortran_usempi_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="use mpi")]

    relative_path = (Path("ompi") / "mpi" / "fortran" / "use-mpi" /
                     "Makefile.am")
    makefile = _first_existing_build_source_path(
        srcdir, builddir, relative_path)
    if makefile is None:
        return [_fail(
            "fortran_usempi_abi_helpers",
            "use mpi Makefile is missing",
            configured=enabled,
            missing_files=[
                _missing_build_source_path(srcdir, builddir, relative_path)
            ])]

    return [_skip(
        "fortran_usempi_abi_helpers",
        SKIP_FORTRAN_HELPERS_SHARED,
        configured=enabled,
        source=str(makefile),
        note="use mpi ABI helpers share the mpif.h helper entry points")]


def _fortran_f08_helper_checks(srcdir, builddir, manifest):
    """Check use mpi_f08 ABI helper templates and generated interfaces.

    This layer is the ABI-relevant Fortran binding for MPI-5.x.  The
    checks look for template inputs and generated interface renames so a
    missing f08 helper is caught during fast source checks.
    """
    enabled = _fortran_enabled(manifest, "use mpi_f08")
    if enabled is False:
        return [_skip("fortran_f08_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="use mpi_f08")]

    base_relative = Path("ompi") / "mpi" / "fortran" / "use-mpi-f08"
    prototype_file = _first_existing_build_source_path(
        srcdir, builddir, base_relative / "Makefile.prototype_files")
    interface_file = _first_existing_build_source_path(
        srcdir, builddir,
        base_relative / "mod" / "mpi-f08-interfaces-generated.h")
    missing_files = []
    missing_patterns = []

    for path, relative_path in (
            (prototype_file, base_relative / "Makefile.prototype_files"),
            (interface_file,
             base_relative / "mod" / "mpi-f08-interfaces-generated.h")):
        if path is None:
            missing_files.append(
                _missing_build_source_path(srcdir, builddir, relative_path))

    prototype_text = (
        _read_text(prototype_file) if prototype_file is not None else ""
    )
    interface_text = (
        _read_text(interface_file) if interface_file is not None else ""
    )
    checked_templates = 0

    for helper in FORTRAN_ABI_HELPERS:
        template_name = helper + ".c.in"
        template_relative = base_relative / template_name
        template_path = _first_existing_build_source_path(
            srcdir, builddir, template_relative)
        mpi_name = "MPI_Abi_" + helper[4:]
        pmpi_name = "PMPI_Abi_" + helper[4:]

        if template_path is None:
            missing_files.append(
                _missing_build_source_path(srcdir, builddir,
                                           template_relative))
            continue

        checked_templates += 1
        template_text = _read_text(template_path)
        requirements = [
            ("prototype file entry", template_name, prototype_text,
             str(prototype_file), False),
            ("PROTOTYPE declaration", "PROTOTYPE", template_text,
             str(template_path), False),
            ("inner C call", "@INNER_CALL@", template_text,
             str(template_path), False),
            ("generated f08 rename", mpi_name + "_f08", interface_text,
             str(interface_file), True),
            ("generated generic rename", mpi_name, interface_text,
             str(interface_file), True),
            ("generated PMPI target", pmpi_name, interface_text,
             str(interface_file), True),
        ]
        for label, pattern, haystack, source, token_match in requirements:
            found = _contains_token(haystack, pattern) if token_match \
                    else pattern in haystack
            if not found:
                missing_patterns.append({
                    "file": source,
                    "pattern": label,
                    "helper": helper,
                })

    if missing_files or missing_patterns:
        return [_fail(
            "fortran_f08_abi_helpers",
            "use mpi_f08 ABI helper source checks failed",
            configured=enabled,
            checked_templates=checked_templates,
            missing_files=missing_files[:20],
            missing_file_count=len(missing_files),
            missing_patterns=missing_patterns[:20],
            missing_pattern_count=len(missing_patterns))]

    return [_pass("fortran_f08_abi_helpers",
                  configured=enabled,
                  checked_templates=checked_templates)]


def _fortran_helper_checks(srcdir, builddir, manifest):
    """Run all configured Fortran ABI helper source checks."""
    checks = []
    checks.extend(_fortran_mpifh_helper_checks(srcdir, builddir, manifest))
    checks.extend(_fortran_usempi_helper_checks(srcdir, builddir, manifest))
    checks.extend(_fortran_f08_helper_checks(srcdir, builddir, manifest))
    return checks


def _fortran_probe_table_unit_check(srcdir):
    """Validate Fortran probe-table contracts in the fast suite.

    Installed Fortran probes are only compiled and run in check-abi, but
    their declarative api_names fields feed the coverage audit.  Keep the
    table honest in make check by verifying that advertised APIs appear
    in the body, stop codes are unique within each executable, source
    rendering consumes all template tokens, and the coverage audit keeps
    PASS/SKIP results partitioned by binding layer.
    """
    checks = []
    for case in (INSTALLED_FORTRAN_COMPILE_PROBES +
                 INSTALLED_FORTRAN_RUNTIME_PROBES):
        body = case.get("body", "")
        missing = sorted(
            name for name in case.get("api_names", ())
            if not _contains_token(body, name)
        )
        if missing:
            checks.append(_fail(
                "fast_fortran_probe_table_unit_checks",
                "Fortran probe api_names entries must appear in the body",
                check="fortran_probe_api_names_in_body",
                probe=case["name"],
                missing=missing))
            return checks

        stop_codes = re.findall(r"(?<![A-Za-z0-9_])stop\s+([0-9]+)",
                                body, re.IGNORECASE)
        duplicates = sorted(
            code for code in set(stop_codes) if stop_codes.count(code) > 1
        )
        if duplicates:
            checks.append(_fail(
                "fast_fortran_probe_table_unit_checks",
                "Fortran probe stop codes must be unique per executable",
                check="fortran_probe_stop_code_uniqueness",
                probe=case["name"],
                duplicates=duplicates))
            return checks

        rendered = _fortran_probe_source(srcdir, case)
        dangling_tokens = sorted(set(re.findall(r"@[A-Z_]+@", rendered)))
        if dangling_tokens:
            checks.append(_fail(
                "fast_fortran_probe_table_unit_checks",
                "Fortran probe rendering left template tokens",
                check="fortran_probe_source_rendering",
                probe=case["name"],
                dangling_tokens=dangling_tokens))
            return checks

    audit_check = _fortran_coverage_audit_unit_check()
    if audit_check["result"] != "PASS":
        checks.append(audit_check)
        return checks

    checks.append(_pass(
        "fast_fortran_probe_table_unit_checks",
        compile_probe_count=len(INSTALLED_FORTRAN_COMPILE_PROBES),
        runtime_probe_count=len(INSTALLED_FORTRAN_RUNTIME_PROBES)))
    return checks


def _fortran_coverage_audit_unit_check():
    """Validate Fortran coverage audit PASS/SKIP accounting."""
    manifest = {
        "configuration": {
            "fortran": {
                "mpif.h": {"enabled": True, "source": "unit"},
                "use mpi": {"enabled": True, "source": "unit"},
                "use mpi_f08": {"enabled": True, "source": "unit"},
            },
        },
        "apis": [
            {
                "name": "MPI_Unit_mpifh_covered",
                "classification": CLASS_IMPLEMENTED,
                "languages": {
                    "mpif.h": True,
                    "use mpi": False,
                    "use mpi_f08": False,
                },
            },
            {
                "name": "MPI_Unit_usempi_skipped",
                "classification": CLASS_IMPLEMENTED,
                "languages": {
                    "mpif.h": False,
                    "use mpi": True,
                    "use mpi_f08": False,
                },
            },
            {
                "name": "MPI_Unit_f08_covered",
                "classification": CLASS_IMPLEMENTED,
                "languages": {
                    "mpif.h": False,
                    "use mpi": False,
                    "use mpi_f08": True,
                },
            },
            {
                "name": "MPI_Unit_f08_pending",
                "classification": CLASS_IMPLEMENTED,
                "languages": {
                    "mpif.h": False,
                    "use mpi": False,
                    "use mpi_f08": True,
                },
            },
        ],
    }
    tools = {"open_mpi": {"mpifort": sys.executable}}
    compile_checks = [
        {
            "name": "compile_mpifh",
            "result": "PASS",
            "language": "mpif.h",
            "api_names": ["MPI_Unit_mpifh_covered"],
            "details": {},
            "skip_reason": None,
        },
        {
            "name": "compile_usempi",
            "result": "SKIP",
            "language": "use mpi",
            "api_names": ["MPI_Unit_usempi_skipped"],
            "details": {},
            "skip_reason": SKIP_FORTRAN_WRAPPER_UNAVAILABLE,
        },
    ]
    runtime_checks = [
        {
            "name": "runtime_f08",
            "result": "PASS",
            "language": "use mpi_f08",
            "api_names": ["MPI_Unit_f08_covered"],
            "details": {},
            "skip_reason": None,
        },
    ]
    audit = _fortran_coverage_audit(
        manifest, tools, compile_checks, runtime_checks)
    languages = audit["details"]["languages"]
    expected = {
        "mpif.h": {
            "covered_implemented_count": 1,
            "pending_phase11b_count": 0,
            "coverage_kind": "regression",
        },
        "use mpi": {
            "covered_implemented_count": 0,
            "pending_phase11b_count": 1,
            "coverage_kind": "regression",
        },
        "use mpi_f08": {
            "covered_implemented_count": 1,
            "pending_phase11b_count": 1,
            "coverage_kind": "standard_abi",
        },
    }
    mismatches = {}
    for language, language_expected in expected.items():
        actual = languages[language]
        for key, value in language_expected.items():
            if actual[key] != value:
                mismatches.setdefault(language, {})[key] = {
                    "expected": value,
                    "actual": actual[key],
                }

    non_f08_standard = [
        language for language, actual in languages.items()
        if language != "use mpi_f08" and
        actual["coverage_kind"] == "standard_abi"
    ]
    if mismatches or non_f08_standard:
        return _fail(
            "fast_fortran_probe_table_unit_checks",
            "Fortran coverage audit synthetic accounting failed",
            check="fortran_coverage_audit",
            mismatches=mismatches,
            non_f08_standard_abi_languages=non_f08_standard,
            audit=audit)
    return _pass(
        "fast_fortran_probe_table_unit_checks",
        check="fortran_coverage_audit")


def _discovery_helper_unit_checks():
    """Run fast unit checks for MPI installation discovery helpers.

    These helpers are intentionally pure or nearly pure so the fast tier
    can catch operator-facing discovery regressions without requiring an
    installed Open MPI or MPICH.
    """
    checks = []
    if _open_mpi_identity(True, False, True) != "open_mpi":
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Explicit Open MPI ABI overrides must bypass layout "
            "heuristics when the wrapper links libmpi_abi",
            check="open_mpi_explicit_override_with_abi_link"))
        return checks
    if _open_mpi_identity(True, True, False) != "unknown":
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Open MPI ABI wrapper selection must reject wrappers that do "
            "not link libmpi_abi",
            check="open_mpi_requires_mpi_abi_link"))
        return checks
    if _open_mpi_identity(False, True, True) != "open_mpi":
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "PATH-discovered Open MPI ABI wrappers with libmpi_abi links "
            "must classify as Open MPI",
            check="open_mpi_path_discovery_with_abi_link"))
        return checks

    if _mpich_identity(False, False, False, False) != "unknown":
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Hydra/CH evidence alone must not classify an install as MPICH",
            check="mpich_identity_without_mpich_marker"))
        return checks
    if _mpich_identity(False, False, True, False) != "mpich":
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Explicit MPICH evidence must classify an install as MPICH",
            check="mpich_identity_with_mpich_marker"))
        return checks
    if _mpich_identity(True, False, False, False) != "mpich":
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Explicit MPICH overrides must bypass weak identity "
            "heuristics when no Open MPI evidence is present",
            check="mpich_explicit_override_without_marker"))
        return checks
    if _mpich_identity(True, True, True, True) != "unknown":
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Open MPI evidence must reject an otherwise MPICH-looking "
            "candidate",
            check="mpich_identity_rejects_open_mpi"))
        return checks

    if not _words_define_macro(["gcc", "-DMPI_ABI"], "MPI_ABI"):
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "MPICH ABI wrapper detection must recognize -DMPI_ABI",
            check="mpich_abi_define_joined"))
        return checks
    if not _words_define_macro(["gcc", "-D", "MPI_ABI"], "MPI_ABI"):
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "MPICH ABI wrapper detection must recognize separated -D "
            "MPI_ABI",
            check="mpich_abi_define_separated"))
        return checks
    if not _words_link_library(["-lmpi_abi"], "mpi_abi"):
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "MPICH ABI wrapper detection must recognize libmpi_abi "
            "linkage",
            check="mpich_links_mpi_abi"))
        return checks
    reasons = _abi_suitability(
        {
            "identity_reason": "wrapper_not_identified_as_mpich",
            "header_reason": "missing_mpi_forum_abi_header",
            "required_libraries": ("mpi_abi",),
            "required_macros": ("MPI_ABI",),
        },
        False,
        False,
        False,
        [],
        None,
        [])["unsuitable_reasons"]
    if "wrapper_does_not_link_libmpi_abi" not in reasons:
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "MPICH ABI suitability diagnostics must name missing "
            "libmpi_abi linkage",
            check="mpich_abi_unsuitable_reasons"))
        return checks
    with tempfile.TemporaryDirectory() as tmpdir:
        libdir = Path(tmpdir)
        (libdir / "libmpi_abi.a").touch()
        complete_suitability = _abi_suitability(
            {
                "identity_reason": "wrapper_not_identified_as_mpich",
                "header_reason": "missing_mpi_forum_abi_header",
                "required_libraries": ("mpi_abi",),
                "required_macros": ("MPI_ABI",),
            },
            False,
            True,
            True,
            ["-DMPI_ABI", "-lmpi_abi"],
            None,
            [str(libdir)])
        if complete_suitability["unsuitable_reasons"]:
            checks.append(_fail(
                "fast_discovery_helper_unit_checks",
                "Complete MPICH ABI evidence must not produce unsuitable "
                "reasons",
                check="mpich_abi_complete_evidence",
                suitability=complete_suitability))
            return checks
    report_candidates = _mpich_report_candidates([
        {
            "candidate_kind": "mpich",
            "implementation": "unknown",
            "valid": False,
            "tools": {"mpicc": "/tmp/openmpi/mpicc_abi"},
            "evidence": {"explicit_override": False},
        },
        {
            "candidate_kind": "mpich",
            "implementation": "mpich",
            "valid": False,
            "tools": {"mpicc": "/tmp/mpich/mpicc"},
            "evidence": {"explicit_override": False},
        },
    ], include_invalid=True)
    if len(report_candidates) != 1:
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "MPICH diagnostics must not report unrelated unknown "
            "implementation candidates",
            check="mpich_invalid_candidate_filter",
            candidates=report_candidates))
        return checks

    report_candidates = _open_mpi_report_candidates([
        {
            "candidate_kind": "open_mpi",
            "implementation": "unknown",
            "valid": False,
            "tools": {"mpicc_abi": "/tmp/openmpi/mpicc_abi"},
            "evidence": {
                "explicit_override": False,
                "is_open_mpi": False,
                "has_standard_abi_header": False,
                "links_mpi_abi": True,
                "has_mpich_header": False,
            },
        },
        {
            "candidate_kind": "open_mpi",
            "implementation": "unknown",
            "valid": False,
            "tools": {"mpicc_abi": "/tmp/mpich/mpicc_abi"},
            "evidence": {
                "explicit_override": False,
                "is_open_mpi": False,
                "has_standard_abi_header": False,
                "links_mpi_abi": True,
                "has_mpich_header": True,
            },
        },
    ], include_invalid=True)
    if len(report_candidates) != 1:
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Open MPI diagnostics must retain unconfirmed Open MPI ABI "
            "candidates without reporting MPICH ABI wrappers as Open MPI",
            check="open_mpi_invalid_candidate_filter",
            candidates=report_candidates))
        return checks

    selection = _cross_direction_selection("mpich-to-ompi,typo")
    if (selection["invalid_values"] != ["typo"] or
            selection["directions"] != ["mpich_compile_open_mpi_run"]):
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Invalid MPICH direction values must be reported",
            check="cross_direction_invalid_value",
            selection=selection))
        return checks
    selection = _cross_direction_selection("both")
    if selection["directions"] != [
            "mpich_compile_open_mpi_run",
            "open_mpi_compile_mpich_run"]:
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "MPICH direction value 'both' must expand to both directions",
            check="cross_direction_both_expansion",
            selection=selection))
        return checks
    selection = _cross_direction_selection("open_mpi_compile_mpich_run")
    if selection["directions"] != ["open_mpi_compile_mpich_run"]:
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Explicit MPICH direction names must select that direction",
            check="cross_direction_explicit_name",
            selection=selection))
        return checks
    selection = _cross_direction_selection("")
    if (selection["invalid_values"] != ["<empty>"] or
            selection["directions"] or not selection["error"]):
        checks.append(_fail(
            "fast_discovery_helper_unit_checks",
            "Empty MPICH direction selection must be a setup error",
            check="cross_direction_empty_value",
            selection=selection))
        return checks

    with tempfile.TemporaryDirectory() as tmpdir:
        fake_mpirun = Path(tmpdir) / "mpirun"
        fake_mpirun.write_text("#!/bin/sh\nexit 0\n")
        os.chmod(str(fake_mpirun), 0o755)
        old_path = os.environ.get("PATH")
        old_mpirun = os.environ.get("MPICH_ABI_TEST_MPIRUN")
        try:
            os.environ["PATH"] = ""
            os.environ["MPICH_ABI_TEST_MPIRUN"] = str(fake_mpirun)
            candidates = _all_named_executables(
                ("mpicc_abi", "mpicc"), "MPICH_ABI_TEST_MPIRUN")
        finally:
            if old_path is None:
                os.environ.pop("PATH", None)
            else:
                os.environ["PATH"] = old_path
            if old_mpirun is None:
                os.environ.pop("MPICH_ABI_TEST_MPIRUN", None)
            else:
                os.environ["MPICH_ABI_TEST_MPIRUN"] = old_mpirun
        if candidates:
            checks.append(_fail(
                "fast_discovery_helper_unit_checks",
                "Launcher overrides must not become compiler candidates",
                check="launcher_override_not_compiler_candidate",
                candidates=candidates))
            return checks
        other_tool = Path(tmpdir) / "other" / "mpicc"
        other_tool.parent.mkdir()
        other_tool.write_text("#!/bin/sh\nexit 0\n")
        os.chmod(str(other_tool), 0o755)
        if _same_tool_directory(str(other_tool), str(fake_mpirun)):
            checks.append(_fail(
                "fast_discovery_helper_unit_checks",
                "Launcher overrides must only pair with same-directory "
                "compiler wrappers",
                check="launcher_override_same_directory_pairing"))
            return checks

    for helper in (
            _fast_mpich_transport_env_check,
            _fast_cross_result_unit_check,
            _fast_linux_resolved_library_check):
        helper_check = helper()
        if helper_check["result"] != "PASS":
            checks.append(helper_check)
            return checks

    checks.append(_pass("fast_discovery_helper_unit_checks"))
    return checks


def _fast_mpich_transport_env_check():
    """Validate documented MPICH transport environment policy."""
    saved = {
        name: os.environ.get(name) for name in (
            "MPICH_ABI_TEST_OFI_PROVIDER",
            "MPICH_ABI_TEST_OFI_IFACE",
            "FI_TCP_IFACE",
            "FI_SOCKETS_IFACE",
            "MPICH_ABI_TEST_UCX_TLS",
            "MPICH_ABI_TEST_UCX_NET_DEVICES",
        )
    }
    try:
        for name in saved:
            os.environ.pop(name, None)

        os.environ["MPICH_ABI_TEST_OFI_IFACE"] = "eth-test0"
        env = _mpich_transport_runtime_env("ch4:ofi")
        if env != {"FI_PROVIDER": "tcp", "FI_TCP_IFACE": "eth-test0"}:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "CH4:OFI transport defaults must use tcp and the "
                "selected interface",
                check="mpich_ofi_transport_default",
                env=env)

        os.environ["MPICH_ABI_TEST_OFI_PROVIDER"] = "sockets"
        env = _mpich_transport_runtime_env("ch4:ofi")
        if env != {
                "FI_PROVIDER": "sockets",
                "FI_SOCKETS_IFACE": "eth-test0"}:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "CH4:OFI sockets override must set FI_SOCKETS_IFACE",
                check="mpich_ofi_sockets_override",
                env=env)

        os.environ["MPICH_ABI_TEST_OFI_PROVIDER"] = "system"
        env = _mpich_transport_runtime_env("ch4:ofi")
        if env:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "CH4:OFI system override must leave FI_* unchanged",
                check="mpich_ofi_system_override",
                env=env)

        os.environ.pop("MPICH_ABI_TEST_OFI_PROVIDER", None)
        os.environ.pop("MPICH_ABI_TEST_OFI_IFACE", None)
        env = _mpich_transport_runtime_env("ch4:ucx")
        if env != {"UCX_TLS": "self,sm"}:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "CH4:UCX transport defaults must use local transports",
                check="mpich_ucx_transport_default",
                env=env)

        os.environ["MPICH_ABI_TEST_UCX_TLS"] = "system"
        env = _mpich_transport_runtime_env("ch4:ucx")
        if env:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "CH4:UCX system override must leave UCX_* unchanged",
                check="mpich_ucx_system_override",
                env=env)

        os.environ["MPICH_ABI_TEST_UCX_TLS"] = "self,sm,tcp"
        os.environ["MPICH_ABI_TEST_UCX_NET_DEVICES"] = "en0"
        env = _mpich_transport_runtime_env("ch4:ucx")
        if env != {"UCX_TLS": "self,sm,tcp", "UCX_NET_DEVICES": "en0"}:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "CH4:UCX overrides must be honored",
                check="mpich_ucx_override",
                env=env)

        if _mpich_transport_runtime_env("ch3:nemesis"):
            return _fail(
                "fast_discovery_helper_unit_checks",
                "Non-CH4 OFI/UCX devices must not receive transport "
                "environment overrides",
                check="mpich_transport_device_gating")
    finally:
        for name, value in saved.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value

    return _pass(
        "fast_discovery_helper_unit_checks",
        check="mpich_transport_env")


def _fast_cross_result_unit_check():
    """Validate cross-direction PASS/SKIP/FAIL aggregation rules."""
    details = {
        "compile_implementation": "mpich",
        "run_implementation": "open_mpi",
    }
    preflight_pass = [_pass("preflight")]
    probe_pass = [_pass("probe")]
    probe_skip = [_skip("probe", SKIP_LINKAGE_INSPECTION_UNAVAILABLE)]
    probe_fail = [_fail("probe", "probe failed")]

    result = _cross_direction_result(
        "mpich_compile_open_mpi_run", details, preflight_pass, probe_skip)
    if result["result"] != "SKIP":
        return _fail(
            "fast_discovery_helper_unit_checks",
            "A direction with no passing cross probes must aggregate to "
            "SKIP even when preflight checks passed",
            check="cross_direction_skip_without_probe_pass",
            result=result)

    result = _cross_direction_result(
        "mpich_compile_open_mpi_run", details, preflight_pass, probe_fail)
    if result["result"] != "FAIL":
        return _fail(
            "fast_discovery_helper_unit_checks",
            "A failing cross probe must make the direction fail",
            check="cross_direction_probe_fail",
            result=result)

    result = _cross_direction_result(
        "mpich_compile_open_mpi_run", details,
        [_fail("preflight", "preflight failed")], probe_pass)
    if result["result"] != "FAIL":
        return _fail(
            "fast_discovery_helper_unit_checks",
            "A failing preflight check must make the direction fail",
            check="cross_direction_preflight_fail",
            result=result)

    result = _cross_direction_result(
        "mpich_compile_open_mpi_run", details, preflight_pass, probe_pass)
    if result["result"] != "PASS":
        return _fail(
            "fast_discovery_helper_unit_checks",
            "A direction with at least one passing cross probe and no "
            "failures must pass",
            check="cross_direction_probe_pass",
            result=result)

    summary = _cross_summary_result(
        [_skip("direction1", SKIP_CROSS_PROBES_NOT_PASSED)],
        {"direction1": details})
    if (summary["result"] != "SKIP" or
            summary["skip_reason"] != SKIP_CROSS_PROBES_NOT_PASSED):
        return _fail(
            "fast_discovery_helper_unit_checks",
            "An all-skip cross-direction set must aggregate to SKIP",
            check="cross_summary_all_skip",
            result=summary)

    summary = _cross_summary_result(
        [_skip("direction1", SKIP_CROSS_PROBES_NOT_PASSED),
         _pass("direction2")],
        {"direction1": details, "direction2": details})
    if summary["result"] != "PASS":
        return _fail(
            "fast_discovery_helper_unit_checks",
            "A cross-direction set with a passing direction and no "
            "failures must pass",
            check="cross_summary_pass",
            result=summary)

    summary = _cross_summary_result(
        [_pass("direction1"), _fail("direction2", "direction failed")],
        {"direction1": details, "direction2": details})
    if summary["result"] != "FAIL":
        return _fail(
            "fast_discovery_helper_unit_checks",
            "Any failing direction must make the cross summary fail",
            check="cross_summary_fail",
            result=summary)

    return _pass(
        "fast_discovery_helper_unit_checks",
        check="cross_result_aggregation")


def _fast_linux_resolved_library_check():
    """Validate linkage parsing used by cross-linkage checks."""
    output = """
        libmpi_abi.so.example => /run/My MPI/lib/libmpi_abi.so.example (0x1)
        libmissing_abi.so => not found
    """
    if (_linux_resolved_library(output, "libmpi_abi") !=
            "/run/My MPI/lib/libmpi_abi.so.example"):
        return _fail(
            "fast_discovery_helper_unit_checks",
            "Linux ldd parsing must recognize spaced, suffixed ABI "
            "library resolutions",
            check="linux_resolved_library_suffix")
    if _linux_resolved_library(output, "libmissing_abi") is not None:
        return _fail(
            "fast_discovery_helper_unit_checks",
            "Linux ldd parsing must report unresolved libraries as None",
            check="linux_resolved_library_not_found")
    if _linux_resolved_library(output, "libmissing_abi") is not None:
        return _fail(
            "fast_discovery_helper_unit_checks",
            "Linux ldd parsing must report absent libraries as None",
            check="linux_resolved_library_absent")
    if not _path_is_run_side_abi_library(
            "/run/My MPI/lib/libmpi_abi.so.anything",
            "/run/My MPI/lib",
            "mpi_abi"):
        return _fail(
            "fast_discovery_helper_unit_checks",
            "Linux runtime-library validation must accept arbitrary ABI "
            "shared-library suffixes in the run-side library directory",
            check="linux_run_side_library_suffix")
    if _path_is_run_side_abi_library(
            "/compile/mpi/lib/libmpi_abi.so.anything",
            "/run/mpi/lib",
            "mpi_abi"):
        return _fail(
            "fast_discovery_helper_unit_checks",
            "Linux runtime-library validation must reject ABI libraries "
            "from the compile-side directory",
            check="linux_run_side_library_directory")
    if (_darwin_otool_install_name(
            "  /run/My MPI/lib/libmpi_abi.dylib "
            "(compatibility version 1.0.0, current version 1.0.0)") !=
            "/run/My MPI/lib/libmpi_abi.dylib"):
        return _fail(
            "fast_discovery_helper_unit_checks",
            "Darwin otool parsing must preserve spaces in install names",
            check="darwin_otool_install_name_spaces")
    with tempfile.TemporaryDirectory() as tmpdir:
        libdir = Path(tmpdir)
        (libdir / "libmpi_abi.a").touch()
        if _cross_runtime_library_target(str(libdir), "mpi_abi") is not None:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "Cross runtime library target lookup must reject static "
                "archives",
                check="cross_runtime_target_static_archive")
        (libdir / "libmpi_abi.so.custom").touch()
        if _cross_runtime_library_target(str(libdir), "mpi_abi") is None:
            return _fail(
                "fast_discovery_helper_unit_checks",
                "Cross runtime library target lookup must accept arbitrary "
                "shared-library suffixes",
                check="cross_runtime_target_shared_suffix")
    return _pass(
        "fast_discovery_helper_unit_checks",
        check="cross_linkage_parsing")


def run_fast_checks(manifest, srcdir, builddir, progress=None):
    """Run in-tree checks that do not require an installed Open MPI."""
    checks = []
    if progress is not None:
        progress.start("fast manifest sanity checks")
    _extend_checks(checks, _manifest_sanity_checks(manifest), progress)

    if progress is not None:
        progress.start("fast standard ABI header constants")
    _extend_checks(
        checks, _header_constant_checks(manifest, srcdir, builddir), progress)

    if progress is not None:
        progress.start("fast header constant parser unit checks")
    _append_check(checks, _header_constant_parser_unit_checks(), progress)

    if progress is not None:
        progress.start("fast ABI converter source checks")
    _extend_checks(checks, _abi_converter_checks(srcdir, builddir), progress)

    if progress is not None:
        progress.start("fast Fortran ABI helper source checks")
    _extend_checks(
        checks, _fortran_helper_checks(srcdir, builddir, manifest), progress)

    if progress is not None:
        progress.start("fast Fortran probe table unit checks")
    _extend_checks(
        checks, _fortran_probe_table_unit_check(srcdir), progress)

    if progress is not None:
        progress.start("fast discovery helper unit checks")
    _extend_checks(checks, _discovery_helper_unit_checks(), progress)

    if progress is not None:
        progress.start("fast legacy tolerance unit checks")
    _append_check(checks, _legacy_tolerance_unit_checks(), progress)

    if progress is not None:
        progress.start("fast C parameter normalization unit checks")
    _append_check(checks, _c_parameter_normalization_unit_checks(), progress)

    if progress is not None:
        progress.start("fast completion gate unit checks")
    _append_check(checks, _completion_gate_unit_checks(), progress)
    return checks
