#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Foundational constants, result primitives, I/O, and shared
helpers for the MPI ABI test runner."""

import json
import os
import sys


CLASS_IMPLEMENTED = "implemented"
CLASS_NOT_IMPLEMENTED = "not_implemented"
CLASS_NOT_IN_STANDARD_ABI = "not_in_standard_abi"
CLASS_UNSUPPORTED_BY_BUILD = "unsupported_by_build"
CLASS_UNSUPPORTED_BY_OPEN_MPI = "unsupported_by_open_mpi"

TEST_NOT_WRITTEN = "test_not_written_yet"
TEST_NOT_APPLICABLE = "not_applicable"
TEST_CALLBACK_DEFERRED = "callback_deferred"

SKIP_STANDARD_ABI_DISABLED = "standard_abi_disabled"
SKIP_OPEN_MPI_TOOLS_UNAVAILABLE = "open_mpi_tools_unavailable"
FAIL_OPEN_MPI_ABI_CLASSIFICATION_UNCONFIRMED = (
    "open_mpi_abi_classification_unconfirmed"
)
SKIP_MPICH_TOOLS_UNAVAILABLE = "mpich_tools_unavailable"
SKIP_HEADER_UNAVAILABLE = "generated_standard_abi_header_unavailable"
SKIP_LINKAGE_INSPECTION_UNAVAILABLE = "linkage_inspection_unavailable"
SKIP_SYMBOL_DIAGNOSTICS_UNAVAILABLE = "symbol_diagnostics_unavailable"
SKIP_FORTRAN_BINDINGS_DISABLED = "fortran_bindings_disabled"
SKIP_FORTRAN_BINDING_DISABLED = "fortran_binding_disabled"
SKIP_FORTRAN_HELPERS_SHARED = "fortran_abi_helpers_shared_with_mpifh"
SKIP_FORTRAN_OPTIONAL_DATATYPES_DEFERRED = (
    "fortran_optional_datatypes_deferred"
)
SKIP_FORTRAN_WRAPPER_UNAVAILABLE = "fortran_wrapper_unavailable"
SKIP_RMA_SUPPORT_DISABLED = "rma_support_disabled"
SKIP_MPI_IO_SUPPORT_DISABLED = "mpi_io_support_disabled"
SKIP_DYNAMIC_PROCESS_DISABLED = "dynamic_process_disabled"
SKIP_DATAREP_UNSUPPORTED = "datarep_unsupported"
SKIP_MPIT_EVENTS_UNAVAILABLE = "mpit_events_unavailable"
SKIP_MPIT_EVENTS_DISABLED = "mpit_events_disabled"
SKIP_MPI_ABORT_TERMINATES_JOB = "mpi_abort_terminates_job"
SKIP_COMM_JOIN_REQUIRES_CONNECTED_FD = "comm_join_requires_connected_fd"
SKIP_PHASE10_CALLBACK_REQUIRED = "phase10_callback_required"
SKIP_CROSS_UNSUPPORTED_PLATFORM = "cross_unsupported_platform"
SKIP_MPICH_DIRECTIONS_INVALID = "mpich_directions_invalid"
SKIP_CROSS_PROBES_NOT_PASSED = "cross_probes_not_passed"
FAIL_CROSS_PROBES_NOT_EXECUTED = "cross_probes_not_executed"

EXPECTED_METADATA_VERSION = "5.0"
EXPECTED_API_COUNT = 567
EXPECTED_CONSTANT_COUNT = 373
DEFAULT_COMMAND_TIMEOUT = 30
MIN_EXPECTED_C_HEADER_PROTOTYPES = 1000

MPI_REMOVED_LEGACY_C_NAMES = {
    "MPI_Attr_delete",
    "MPI_Attr_get",
    "MPI_Attr_put",
    "MPI_Keyval_create",
    "MPI_Keyval_free",
    "PMPI_Attr_delete",
    "PMPI_Attr_get",
    "PMPI_Attr_put",
    "PMPI_Keyval_create",
    "PMPI_Keyval_free",
}

MPI_REMOVED_LEGACY_CONSTANT_NAMES = {
    "MPI_DUP_FN",
    "MPI_NULL_COPY_FN",
    "MPI_NULL_DELETE_FN",
}

FORTRAN_BINDING_LANGUAGES = (
    "mpif.h",
    "use mpi",
    "use mpi_f08",
)

ANSI_RED = "\033[0;31m"
ANSI_GREEN = "\033[0;32m"
ANSI_BLUE = "\033[1;34m"
ANSI_MAGENTA = "\033[0;35m"
ANSI_RESET = "\033[m"

VALID_CLASSIFICATIONS = {
    CLASS_IMPLEMENTED,
    CLASS_NOT_IMPLEMENTED,
    CLASS_NOT_IN_STANDARD_ABI,
    CLASS_UNSUPPORTED_BY_BUILD,
    CLASS_UNSUPPORTED_BY_OPEN_MPI,
}

VALID_TEST_STATUSES = {
    TEST_NOT_WRITTEN,
    TEST_NOT_APPLICABLE,
    TEST_CALLBACK_DEFERRED,
}

# Static metadata expectations.  These are intentionally hard gates.  The
# runner is generated against the current docs/ metadata shape, and a
# metadata version/count change means the MPI Forum ABI/API authority has
# changed underneath us.  Failing here is preferable to silently accepting
# a new standard constant or API without reviewing the generated probes,
# task list, skip policy, and completion gate.
ABI_CONVERTER_HANDLES = (
    ("Comm", "comm"),
    ("Errhandler", "errhandler"),
    ("File", "file"),
    ("Group", "group"),
    ("Info", "info"),
    ("Message", "message"),
    ("Op", "op"),
    ("Request", "request"),
    ("Session", "session"),
    ("Type", "type"),
    ("Win", "win"),
)

ABI_CONVERTER_KIND_BY_C_TYPE = {
    "MPI_Comm": "Comm",
    "MPI_Datatype": "Type",
    "MPI_Errhandler": "Errhandler",
    "MPI_File": "File",
    "MPI_Group": "Group",
    "MPI_Info": "Info",
    "MPI_Message": "Message",
    "MPI_Op": "Op",
    "MPI_Request": "Request",
    "MPI_Session": "Session",
    "MPI_Win": "Win",
}

ABI_CONVERTER_REQUIRED_KINDS = tuple(
    kind for kind, _stem in ABI_CONVERTER_HANDLES)

FORTRAN_ABI_HELPERS = (
    "abi_get_fortran_booleans",
    "abi_get_fortran_info",
    "abi_get_info",
    "abi_get_version",
    "abi_set_fortran_booleans",
    "abi_set_fortran_info",
)


def _read_json(path):
    with path.open("r", encoding="utf-8") as stream:
        return json.load(stream)


def _read_text(path):
    return path.read_text(encoding="utf-8", errors="ignore")


def _write_json(path, payload):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        json.dump(payload, stream, indent=2, sort_keys=True)
        stream.write("\n")


def _write_text(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        stream.write(text)


def _build_source_candidates(srcdir, builddir, relative_path):
    """Return build-tree/source-tree candidates for one repository path.

    VPATH builds keep generated files under the build tree while checked-in
    templates remain in the source tree.  Fast source-contract checks need to
    see both layouts: builddir first catches generated VPATH outputs, and
    srcdir preserves normal in-tree and dist-tarball behavior.
    """
    candidates = []
    for root in (builddir, srcdir):
        path = root / relative_path
        if path not in candidates:
            candidates.append(path)
    return candidates


def _first_existing_build_source_path(srcdir, builddir, relative_path):
    """Return the first existing build/source candidate for relative_path."""
    for path in _build_source_candidates(srcdir, builddir, relative_path):
        if path.exists():
            return path
    return None


def _missing_build_source_path(srcdir, builddir, relative_path):
    """Describe all searched candidates for a missing build/source path."""
    return " or ".join(
        str(path)
        for path in _build_source_candidates(srcdir, builddir, relative_path)
    )


def _probe_body_path(srcdir, case):
    """Return the checked-in C body snippet path for one probe case."""
    body_file = case.get("body_file")
    if body_file is None:
        return None
    return srcdir / "test" / "mpi-abi" / body_file


def _probe_body_text(srcdir, case):
    """Load the C body for one installed probe case.

    Long C snippets live in checked-in *.cbody.in files so the Python
    tables remain readable metadata contracts.  Keep a temporary inline
    fallback for older cases while preserving a one-body-source rule:
    exactly one of body or body_file may be present.
    """
    has_body = "body" in case
    has_body_file = "body_file" in case
    if has_body == has_body_file:
        raise RuntimeError(
            "probe {0} must define exactly one of body or body_file".
            format(case.get("name", "<unknown>")))
    if has_body:
        return case["body"]

    path = _probe_body_path(srcdir, case)
    if not path.exists():
        raise RuntimeError(
            "probe {0} body file is missing: {1}".format(
                case.get("name", "<unknown>"), path))
    return _read_text(path)


def _probe_prologue_text(srcdir, case):
    """Load optional top-level C helpers for one installed probe case.

    Most checked-in snippets are straight-line code inserted inside
    main().  Callback probes also need file-scope callback functions.
    Keeping those helpers in a separate prologue file preserves the
    body file as the readable description of the MPI runtime flow.
    """
    prologue_file = case.get("prologue_file")
    if prologue_file is None:
        return ""
    path = srcdir / "test" / "mpi-abi" / prologue_file
    if not path.exists():
        raise RuntimeError(
            "probe {0} prologue file is missing: {1}".format(
                case.get("name", "<unknown>"), path))
    return _read_text(path)


def _check_result(name, result, details=None, reason=None):
    """Create the normalized check result object used in reports."""
    return {
        "name": name,
        "result": result,
        "details": details or {},
        "skip_reason": reason,
    }


def _fail(name, message, **details):
    payload = {"message": message}
    payload.update(details)
    return _check_result(name, "FAIL", payload)


def _pass(name, **details):
    return _check_result(name, "PASS", details)


def _skip(name, reason, **details):
    return _check_result(name, "SKIP", details, reason)


def _color_tests_enabled(setting):
    """Resolve Automake-compatible color-test settings."""
    if setting in ("yes", "always"):
        return True
    if setting in ("no", "never"):
        return False

    env_setting = os.environ.get("OMPI_ABI_TEST_COLOR_TESTS")
    if env_setting is None:
        env_setting = os.environ.get("AM_COLOR_TESTS")
    if env_setting is not None:
        env_setting = env_setting.strip().lower()
        if env_setting in ("yes", "always"):
            return True
        if env_setting in ("no", "never"):
            return False

    return os.environ.get("TERM") != "dumb" and sys.stdout.isatty()


class _Colors:
    """Apply Automake-style colors to progress lines when enabled."""

    def __init__(self, enabled):
        self.enabled = enabled

    def result(self, status, text):
        if not self.enabled:
            return text
        color = {
            "PASS": ANSI_GREEN,
            "SKIP": ANSI_BLUE,
            "FAIL": ANSI_RED,
            "ERROR": ANSI_MAGENTA,
        }.get(status)
        if color is None:
            return text
        return color + text + ANSI_RESET


class _Progress:
    """Emit make-check-style TEST/PASS/SKIP/FAIL progress lines."""

    def __init__(self, enabled, colors):
        self.enabled = enabled
        self.colors = colors

    def start(self, name):
        if self.enabled:
            print("TEST: {0}".format(name), flush=True)

    def check(self, check):
        if not self.enabled:
            return
        line = "{0}: {1}".format(check["result"], check["name"])
        if check["skip_reason"]:
            line += " ({0})".format(check["skip_reason"])
        print(self.colors.result(check["result"], line), flush=True)


def _append_check(checks, check, progress):
    checks.append(check)
    if progress is not None:
        progress.check(check)


def _extend_checks(checks, new_checks, progress):
    checks.extend(new_checks)
    if progress is not None:
        for check in new_checks:
            progress.check(check)


def _command_timeout():
    """Return the timeout used for compile, launcher, and inspection jobs.

    MPI launch failures often manifest as hangs rather than immediate
    errors, especially around two-rank barriers or mismatched launcher
    environments.  A timeout turns those hangs into ordinary FAIL records
    with command logs instead of wedging make check-abi indefinitely.
    """
    return int(os.environ.get("OMPI_ABI_TEST_TIMEOUT",
                              str(DEFAULT_COMMAND_TIMEOUT)))
