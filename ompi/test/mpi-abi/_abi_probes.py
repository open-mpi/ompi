#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Shared analysis layer: header/constant parsing, coverage
audits, and probe generation used by the fast, installed,
and cross checks."""

import ast
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    ABI_CONVERTER_KIND_BY_C_TYPE, ABI_CONVERTER_REQUIRED_KINDS,
    CLASS_IMPLEMENTED, SKIP_COMM_JOIN_REQUIRES_CONNECTED_FD,
    SKIP_DYNAMIC_PROCESS_DISABLED, SKIP_MPIT_EVENTS_DISABLED,
    SKIP_MPI_ABORT_TERMINATES_JOB, SKIP_MPI_IO_SUPPORT_DISABLED,
    SKIP_PHASE10_CALLBACK_REQUIRED, SKIP_RMA_SUPPORT_DISABLED, _fail, _pass,
    _probe_body_text, _probe_prologue_text, _read_text)
from _abi_tables import (
    CALLBACK_ATTRIBUTE_API_NAMES, LEGACY_ATTRIBUTE_API_NAMES,
    DEFERRED_CALLBACK_API_NAMES)


# A standard ABI mpi.h is large (the suite requires >= 1000 prototypes) and
# is parsed several times per cross direction.  Its contents are stable for
# the lifetime of one runner process, so memoize the parse results by header
# path to avoid repeated full-file reads and regex passes.
_HEADER_PROTOTYPE_CACHE = {}
_HEADER_CONSTANT_NAME_CACHE = {}


def _strip_c_comments(value):
    value = re.sub(r"/\*.*?\*/", " ", value)
    return value.split("//", 1)[0]


def _strip_c_integer_suffixes(value):
    return re.sub(
        r"\b(0[xX][0-9a-fA-F]+|[0-9]+)(?:[uUlL]+)\b", r"\1", value)


def _strip_c_casts(value):
    type_name = r"(?:const\s+|volatile\s+)?[A-Za-z_]\w*"
    type_expr = type_name + r"(?:\s+" + type_name + r")*\s*\**"
    return re.sub(r"\(\s*" + type_expr + r"\s*\)", " ", value)


def _eval_integer_expression_node(node):
    """Evaluate a restricted Python AST mirroring C integer expressions.

    Header constants may be expressions such as shifts or bitwise ORs,
    not just literal integers.  Using Python's AST gives us structured
    parsing without eval(); only integer literals and arithmetic/bitwise
    operators that can appear in ABI constants are accepted.
    """
    if isinstance(node, ast.Expression):
        return _eval_integer_expression_node(node.body)
    if isinstance(node, ast.Constant) and type(node.value) is int:
        return node.value
    ast_num = getattr(ast, "Num", None)
    if (ast_num is not None and isinstance(node, ast_num) and
            type(node.n) is int):
        return node.n
    if isinstance(node, ast.UnaryOp):
        operand = _eval_integer_expression_node(node.operand)
        if operand is None:
            return None
        if isinstance(node.op, ast.UAdd):
            return operand
        if isinstance(node.op, ast.USub):
            return -operand
        if isinstance(node.op, ast.Invert):
            return ~operand
        return None
    if isinstance(node, ast.BinOp):
        left = _eval_integer_expression_node(node.left)
        right = _eval_integer_expression_node(node.right)
        if left is None or right is None:
            return None
        if isinstance(node.op, ast.Add):
            return left + right
        if isinstance(node.op, ast.Sub):
            return left - right
        if isinstance(node.op, ast.Mult):
            return left * right
        if isinstance(node.op, ast.Mod):
            return left % right
        if isinstance(node.op, ast.BitOr):
            return left | right
        if isinstance(node.op, ast.BitAnd):
            return left & right
        if isinstance(node.op, ast.BitXor):
            return left ^ right
        if isinstance(node.op, ast.LShift) and right >= 0:
            return left << right
        if isinstance(node.op, ast.RShift) and right >= 0:
            return left >> right
        return None
    return None


def _extract_integer_constant(value):
    """Parse a C integer constant expression into an int when safe.

    This intentionally rejects expressions with identifiers such as
    MPI_VERSION because evaluating those would require a preprocessor
    model.  Rejecting and reporting an unparsed constant is safer than a
    regex that happens to pull one integer token out of the wrong place.
    """
    value = _strip_c_integer_suffixes(_strip_c_casts(_strip_c_comments(value)))
    if re.search(r"\bMPI_VERSION\b", value):
        return None
    if re.search(r"[^0-9a-fA-FxX\s()+\-*%<>&|^~]", value):
        return None
    try:
        tree = ast.parse(value, mode="eval")
    except SyntaxError:
        return None
    return _eval_integer_expression_node(tree)


def _metadata_integer_value(value):
    """Normalize an ABI metadata integer value into an int."""
    if isinstance(value, int):
        return value
    if isinstance(value, str):
        try:
            return int(value, 0)
        except ValueError:
            return None
    return None


def _parse_header_constants(path):
    """Parse numeric MPI constants from a standard ABI mpi.h.

    This parser is for semantic value comparison, so it only records
    constants whose expressions can be reduced to integers.  Constants
    that are declared but not numerically parseable are tracked as
    unparsed so the header check can fail with a concrete reason.
    """
    constants = {}
    unparsed = {}
    aliases = {}
    define_re = re.compile(r"^\s*#define\s+(MPI\w+)\s+(.+?)\s*$")
    enum_re = re.compile(r"^\s*(MPI\w+)\s*=\s*([^,}]+),?\s*(?:/\*.*)?$")
    implicit_enum_re = re.compile(r"^\s*(MPI\w+)\s*,")
    next_enum_value = None
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        match = define_re.match(line)
        if match is not None:
            name, value = match.groups()
            integer = _extract_integer_constant(value)
            if integer is not None:
                constants[name] = integer
            elif re.match(r"^MPI\w+$", value.strip()):
                aliases[name] = value.strip()
            else:
                unparsed[name] = value
            continue

        match = enum_re.match(line)
        if match is not None:
            name, value = match.groups()
            integer = _extract_integer_constant(value)
            if integer is not None:
                constants[name] = integer
                next_enum_value = integer + 1
            else:
                unparsed[name] = value
                next_enum_value = None
            continue

        match = implicit_enum_re.match(line)
        if match is not None and next_enum_value is not None:
            constants[match.group(1)] = next_enum_value
            next_enum_value += 1
            continue

        if "}" in line:
            next_enum_value = None

    unresolved = True
    while unresolved:
        unresolved = False
        for name, target in list(aliases.items()):
            if name in constants:
                aliases.pop(name, None)
                continue
            if target in constants:
                constants[name] = constants[target]
                aliases.pop(name, None)
                unresolved = True
    unparsed.update(aliases)
    return constants, unparsed


def _compare_header_constants_to_metadata(manifest, header_constants,
                                          unparsed_header_constants):
    """Partition ABI metadata constants against parsed header values.

    Both the fast in-tree header check and the installed cross-header
    check share this comparison so their scoping rules (skip when the
    metadata value or C type is missing, or the entry is a deprecated
    function) and their missing/unparsed/mismatch partitioning cannot
    drift apart.  Callers turn the returned dict into their own
    PASS/FAIL result.
    """
    missing = []
    mismatches = []
    unparsed = []
    skipped = []
    checked = 0
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
    return {
        "missing": missing,
        "mismatches": mismatches,
        "unparsed": unparsed,
        "skipped": skipped,
        "checked": checked,
    }


def _parse_header_constant_names(path):
    """Parse declared MPI constant names, including implicit enum members.

    Runtime probes need to know whether mpi.h declares a constant, not
    whether its value can be evaluated.  Keep this parser broader than
    _parse_header_constants() so an implicit enum member is not silently
    dropped from the installed-header declaration preflight.
    """
    key = str(path)
    cached = _HEADER_CONSTANT_NAME_CACHE.get(key)
    if cached is not None:
        return set(cached)
    names = set()
    define_re = re.compile(r"^\s*#define\s+(MPI\w+)\b")
    enum_re = re.compile(r"^\s*(MPI\w+)\b\s*(?:=|,)")
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        match = define_re.match(line)
        if match is None:
            match = enum_re.match(line)
        if match is not None:
            names.add(match.group(1))
    _HEADER_CONSTANT_NAME_CACHE[key] = names
    return set(names)


def _constant_sort_key(entry):
    """Sort constants by ABI value first, then by name."""
    abi_value = _metadata_integer_value(entry["abi_value"])
    if abi_value is None:
        abi_value = sys.maxsize
    return (abi_value, entry["name"])


def _metadata_constant_entries(manifest):
    """Return implemented standard ABI constants from the manifest."""
    return [
        entry for entry in manifest["constants"]
        if entry["classification"] == CLASS_IMPLEMENTED
    ]


def _is_fortran_datatype_constant(entry):
    """Return whether an MPI_Datatype metadata entry is Fortran-only.

    C and Fortran predefined datatypes share the MPI_Datatype C handle
    type, but they are not governed by the same availability rules.
    Separating them lets C datatype probes stay exhaustive while the
    Fortran datatype probe remains gated on configured Fortran support.
    """
    if entry["c_type"] != "MPI_Datatype":
        return False
    if entry["category"] in (
            "OPT_DATATYPES_FORTRAN",
            "REDUCTION_FUNC_DATATYPES_FORTRAN"):
        return True
    datatypes = entry.get("datatypes", {})
    return "c" not in datatypes and (
        "f90" in datatypes or "f08" in datatypes
    )


def _is_c_predefined_handle_constant(entry):
    """Return whether a constant can use a C handle converter.

    The toint/fromint converters apply to MPI object handles, not every
    ABI constant.  For MPI_Datatype, exclude Fortran-only datatypes here
    so they are checked by the Fortran-specific probe and skip policy.
    """
    if entry["c_type"] not in ABI_CONVERTER_KIND_BY_C_TYPE:
        return False
    if entry["c_type"] == "MPI_Datatype":
        return not _is_fortran_datatype_constant(entry)
    return True


def _generated_check_lines(entries, template):
    """Render metadata-selected constants into C probe statements.

    Empty output is allowed here because the caller may be assembling a
    probe that is skipped in the current configuration.  Nonempty family
    requirements are enforced by _runtime_probe_generation_check() before
    installed probes are compiled, where the failure can name the family.
    """
    lines = []
    for entry in sorted(entries, key=_constant_sort_key):
        lines.append(template.format(
            name=entry["name"],
            kind=ABI_CONVERTER_KIND_BY_C_TYPE.get(entry["c_type"])))
    if not lines:
        return "    /* No metadata/header-declared constants to check. */"
    return "\n".join(lines)


def _predefined_handle_entries(manifest):
    """Return metadata constants tested by predefined handle probes."""
    return [
        entry for entry in _metadata_constant_entries(manifest)
        if _is_c_predefined_handle_constant(entry)
    ]


def _fortran_datatype_entries(manifest):
    """Return metadata constants tested by Fortran datatype probes."""
    return [
        entry for entry in _metadata_constant_entries(manifest)
        if _is_fortran_datatype_constant(entry)
    ]


def _declared_fortran_datatype_entries(manifest, declared_names):
    """Return Fortran datatype constants declared by installed mpi.h.

    Optional Fortran datatype availability is compiler/configuration
    dependent.  Unlike C handle constants, the current Phase 8 probe
    covers the subset the installed standard ABI header actually
    declares; unavailable optional datatype behavior is tracked by a
    later task.
    """
    return [
        entry for entry in _fortran_datatype_entries(manifest)
        if entry["name"] in declared_names
    ]


def _error_class_entries(manifest):
    """Return MPI_ERR_* classes that should round-trip via MPI_Error_class."""
    return [
        entry for entry in _metadata_constant_entries(manifest)
        if entry["category"] == "ERROR_CLASSES"
        and entry["name"].startswith("MPI_ERR_")
        and entry["name"] != "MPI_ERR_LASTCODE"
    ]


def _predefined_handle_counts_by_kind(entries):
    """Count generated predefined-handle checks per converter kind."""
    counts = {}
    for entry in entries:
        kind = ABI_CONVERTER_KIND_BY_C_TYPE[entry["c_type"]]
        counts[kind] = counts.get(kind, 0) + 1
    return counts


def _predefined_handle_decls(manifest):
    """Generate C counters for each predefined-handle converter kind."""
    counts = _predefined_handle_counts_by_kind(
        _predefined_handle_entries(manifest))
    lines = []
    for kind in sorted(counts):
        lines.append("    int predefined_{0}_checks = 0;".format(kind))
    return "\n".join(lines)


def _predefined_handle_guards(manifest):
    """Generate C guards that enforce exact per-kind check counts.

    A single aggregate counter is not enough: if a parser or metadata
    change dropped every MPI_Win constant, other handle kinds could keep
    the aggregate nonzero.  Per-kind guards make that erosion visible.
    """
    counts = _predefined_handle_counts_by_kind(
        _predefined_handle_entries(manifest))
    lines = []
    for kind in sorted(counts):
        lines.extend([
            "    if ({0} != predefined_{1}_checks) {{".format(
                counts[kind], kind),
            "        fprintf(stderr, "
            "\"ABI_FAIL:predefined:{0}:count:%d:%d\\n\",".format(kind),
            "                {0}, predefined_{1}_checks);".format(
                counts[kind], kind),
            "        ret = MPI_Finalize();",
            "        if (MPI_SUCCESS != ret) {",
            "            return 21;",
            "        }",
            "        return 22;",
            "    }",
        ])
    return "\n".join(lines)


def _predefined_handle_checks(manifest):
    """Generate predefined-handle converter round-trip statements."""
    return _generated_check_lines(
        _predefined_handle_entries(manifest),
        "    ABI_CHECK_PREDEFINED_HANDLE({kind}, {name});")


def _fortran_datatype_checks(manifest, declared_names):
    """Generate Fortran datatype converter round-trip statements."""
    entries = _declared_fortran_datatype_entries(manifest, declared_names)
    return _generated_check_lines(
        entries,
        "    ABI_CHECK_FORTRAN_TYPE({name});")


def _error_class_checks(manifest):
    """Generate MPI_Error_class statements for predefined error classes."""
    entries = _error_class_entries(manifest)
    return _generated_check_lines(
        entries,
        "    ABI_CHECK_ERROR_CLASS({name});")


def _runtime_probe_constant_names(manifest, include_fortran, declared_names):
    """Return constants expected in installed mpi.h for runtime probes."""
    entries = (
        _predefined_handle_entries(manifest) +
        _error_class_entries(manifest)
    )
    if include_fortran:
        entries += _declared_fortran_datatype_entries(
            manifest, declared_names)
    return set(entry["name"] for entry in entries)


def _runtime_probe_generation_check(manifest, include_fortran,
                                    declared_names,
                                    check_name="installed_c_probe_generation"):
    """Fail if metadata-derived runtime probe families would be empty.

    The runtime probes are metadata-driven so future constants are picked
    up automatically.  That same indirection can hide mistakes if a
    classification rule starts returning an empty family; this preflight
    fails before C generation so an empty generated block is never
    accepted as an intentional zero-check probe.
    """
    predefined_entries = _predefined_handle_entries(manifest)
    predefined_counts = _predefined_handle_counts_by_kind(predefined_entries)
    missing_kinds = [
        kind for kind in ABI_CONVERTER_REQUIRED_KINDS
        if predefined_counts.get(kind, 0) == 0
    ]
    error_class_count = len(_error_class_entries(manifest))
    fortran_type_count = (
        len(_declared_fortran_datatype_entries(manifest, declared_names))
        if include_fortran else None
    )

    if missing_kinds or error_class_count == 0 or (
            include_fortran and fortran_type_count == 0):
        return _fail(
            check_name,
            "metadata-derived converter probe family is empty",
            missing_predefined_handle_kinds=missing_kinds,
            predefined_handle_counts=predefined_counts,
            error_class_count=error_class_count,
            fortran_type_count=fortran_type_count)

    return _pass(
        check_name,
        predefined_handle_counts=predefined_counts,
        error_class_count=error_class_count,
        fortran_type_count=fortran_type_count)


def _runtime_api_probe_api_names(cases, include_support=False):
    """Return API names referenced by installed runtime API probes.

    api_names is the primary coverage set.  support_api_names records
    setup/teardown calls such as MPI_Init or MPI_Comm_rank that should be
    validated against metadata and the header but not counted as the
    probe's advertised family coverage.
    """
    names = set()
    for case in cases:
        names.update(case.get("api_names", ()))
        if include_support:
            names.update(case.get("support_api_names", ()))
    return names


def _runtime_api_probe_family_counts(cases):
    """Count primary runtime API probe coverage by declared family."""
    counts = {}
    for case in cases:
        family = case["family"]
        counts[family] = counts.get(family, 0) + len(case["api_names"])
    return counts


def _runtime_audit_work_package(entry):
    """Return the Phase 9/10 work package that owns an uncovered API.

    This audit is deliberately advisory until the completion gate is
    enabled.  It gives future implementation chunks a metadata-derived
    missing list without pretending that every implemented C API needs a
    runtime probe: ABI helpers and integer handle converters are covered
    by earlier phases, callback APIs belong to Phase 10, and some APIs
    need explicit skip/defer policy rather than local CI execution.
    """
    name = entry["name"]
    family = entry["family"]
    if entry["callback"]:
        return "phase10_callback"
    if name.startswith("MPI_Abi_"):
        return "phase8_abi_helper"
    if name.endswith("_toint") or name.endswith("_fromint"):
        return "phase8_converter"
    if name in (
        "MPI_Comm_delete_attr",
        "MPI_Comm_free_keyval",
        "MPI_Comm_set_attr",
        "MPI_Grequest_complete",
        "MPI_Op_free",
        "MPI_Type_delete_attr",
        "MPI_Type_free_keyval",
        "MPI_Type_get_attr",
        "MPI_Type_set_attr",
        "MPI_Win_delete_attr",
        "MPI_Win_free_keyval",
        "MPI_Win_set_attr",
    ):
        return "phase10_callback"
    if name in (
        "MPI_Comm_accept",
        "MPI_Comm_connect",
        "MPI_Comm_disconnect",
        "MPI_Comm_get_parent",
        "MPI_Comm_join",
        "MPI_Comm_spawn",
        "MPI_Comm_spawn_multiple",
    ):
        return "chunk9h_dynamic_process"
    if name == "MPI_Abort":
        return "chunk9i_intentional_abort"
    if family in (
        "point_to_point", "bsend", "ibsend", "ssend", "issend",
        "rsend", "irsend", "sendrecv", "isendrecv", "mprobe",
        "improbe", "mrecv", "imrecv", "message", "psend", "precv",
        "pready", "parrived", "request", "wait", "waitall",
        "waitany", "waitsome", "test", "testall", "testany",
        "testsome", "start", "startall", "cancel", "buffer",
    ):
        return "chunk9b_point_to_point_request"
    if family in (
        "cart", "cartdim", "graph", "graphdims", "dist", "dims",
        "topo", "neighbor", "ineighbor",
    ):
        return "chunk9c_topology_neighbor"
    if family in (
        "collective", "alltoallv", "alltoallw", "iallgather",
        "iallgatherv", "iallreduce", "ialltoall", "ialltoallv",
        "ialltoallw", "ibarrier", "ibcast", "iexscan", "igather",
        "igatherv", "ireduce", "iscan", "iscatter", "iscatterv",
        "scatter", "scatterv",
    ):
        return "chunk9d_collective"
    if family in ("type", "status", "pack", "unpack", "op"):
        return "chunk9e_datatype_status_pack_op"
    if family in ("comm", "group", "intercomm"):
        return "chunk9_comm_group_remaining"
    if family in ("win", "rma", "compare", "fetch", "raccumulate"):
        return "chunk9f_rma_window"
    if family == "file":
        return "chunk9g_mpi_io"
    if family in (
        "open", "close", "publish", "lookup", "unpublish",
        "spawn", "connect", "accept",
    ):
        return "chunk9h_dynamic_process"
    if family == "mpi_t":
        return "chunk9i_mpi_t"
    return "chunk9i_misc"


def _runtime_explicit_skip_reason(name, package):
    """Return the stable reason for APIs intentionally not run in Phase 9."""
    if package == "phase10_callback":
        return SKIP_PHASE10_CALLBACK_REQUIRED
    if name == "MPI_Abort":
        return SKIP_MPI_ABORT_TERMINATES_JOB
    if name == "MPI_Comm_join":
        return SKIP_COMM_JOIN_REQUIRES_CONNECTED_FD
    return None


def _runtime_api_coverage_audit(manifest, header, cases):
    """Report implemented C APIs not yet covered by runtime probes.

    The check result is PASS during phased development.  The completion
    gate is the place that eventually turns any remaining
    test_not_written_yet coverage into a hard failure.  Keeping this as
    an installed check means it audits against the same standard ABI
    header that runtime probes compile against.
    """
    prototypes = _parse_c_header_prototypes(header)
    declared_names = set(prototypes)
    runtime_names = _runtime_api_probe_api_names(
        cases, include_support=True)
    missing_by_package = {}
    explicit_skips_by_package = {}
    covered = 0
    explicitly_skipped = 0
    skipped_not_declared = 0

    for entry in manifest["apis"]:
        if entry["classification"] != CLASS_IMPLEMENTED:
            continue
        if not entry["languages"]["c"]:
            continue
        name = entry["name"]
        if name not in declared_names:
            skipped_not_declared += 1
            continue
        package = _runtime_audit_work_package(entry)
        if package in ("phase8_abi_helper", "phase8_converter"):
            covered += 1
            continue
        if name in runtime_names:
            covered += 1
            continue
        skip_reason = _runtime_explicit_skip_reason(name, package)
        if skip_reason is not None:
            explicitly_skipped += 1
            explicit_skips_by_package.setdefault(package, []).append({
                "name": name,
                "skip_reason": skip_reason,
            })
            continue
        missing_by_package.setdefault(package, []).append(name)

    summarized = {
        package: names[:20]
        for package, names in sorted(missing_by_package.items())
    }
    counts = {
        package: len(names)
        for package, names in sorted(missing_by_package.items())
    }
    skip_counts = {
        package: len(items)
        for package, items in sorted(explicit_skips_by_package.items())
    }
    summarized_skips = {
        package: items[:20]
        for package, items in sorted(explicit_skips_by_package.items())
    }
    return _pass(
        "installed_c_runtime_api_coverage_audit",
        covered=covered,
        explicitly_skipped=explicitly_skipped,
        explicit_skips_by_package=summarized_skips,
        explicit_skips_by_package_counts=skip_counts,
        missing_count=sum(counts.values()),
        missing_by_package=summarized,
        missing_by_package_counts=counts,
        skipped_not_declared=skipped_not_declared)


def _runtime_api_probe_body_calls(srcdir, case):
    """Return MPI API calls made by a runtime probe source.

    The runtime probe table is declarative: api_names/support_api_names
    tell reports what a C body and its optional file-scope prologue are
    supposed to exercise.  Keep that declaration tied to the generated
    source by checking for direct MPI_* function calls in both snippets.
    If later probes need function pointers or macro indirection, they
    should extend this guard instead of weakening the coverage contract.
    """
    call_re = re.compile(r"(?<![A-Za-z0-9_])(MPI_[A-Za-z0-9_]+)\s*\(")
    source = _probe_prologue_text(srcdir, case)
    source += "\n"
    source += _probe_body_text(srcdir, case)
    return set(call_re.findall(source))


def _runtime_api_probe_generation_check(
        srcdir, manifest, header, cases,
        check_name="installed_c_runtime_api_probe_generation"):
    """Validate the runtime API probe table before compiling C sources.

    Phase 9 starts with seed API-family probes, not exhaustive family
    coverage.  Even so, each seed probe names its intended MPI API
    coverage explicitly.  This preflight keeps that contract honest by
    checking that every named API exists in docs/ metadata, is classified
    as implemented in this tree, and is declared by the installed
    standard ABI header that mpicc_abi will use for the generated source.
    It also checks the declared coverage against the source body so a
    stale api_names entry cannot claim coverage for a call that was
    removed.
    """
    entries_by_name = {
        entry["name"]: entry for entry in manifest["apis"]
    }
    primary_names = _runtime_api_probe_api_names(cases)
    all_names = _runtime_api_probe_api_names(cases, include_support=True)
    prototypes = _parse_c_header_prototypes(header)
    declared_names = set(prototypes)

    missing_metadata = sorted(
        name for name in all_names if name not in entries_by_name)
    not_implemented = sorted(
        name for name in all_names
        if name in entries_by_name and
        entries_by_name[name]["classification"] != CLASS_IMPLEMENTED)
    missing_header = sorted(name for name in all_names
                            if name not in declared_names)

    empty_cases = sorted(case["name"] for case in cases
                         if not case.get("api_names"))

    missing_body_calls = {}
    undeclared_body_calls = {}
    body_file_errors = {}
    prologue_file_errors = {}
    for case in cases:
        advertised = (
            set(case.get("api_names", ())) |
            set(case.get("support_api_names", ()))
        )
        try:
            _probe_prologue_text(srcdir, case)
        except RuntimeError as exc:
            prologue_file_errors[case["name"]] = str(exc)
        try:
            called = _runtime_api_probe_body_calls(srcdir, case)
        except RuntimeError as exc:
            body_file_errors[case["name"]] = str(exc)
            continue
        missing = sorted(advertised - called)
        undeclared = sorted(called - advertised)
        if missing:
            missing_body_calls[case["name"]] = missing
        if undeclared:
            undeclared_body_calls[case["name"]] = undeclared

    if (missing_metadata or not_implemented or missing_header or
            empty_cases or missing_body_calls or undeclared_body_calls or
            body_file_errors or prologue_file_errors):
        return _fail(
            check_name,
            "runtime API probe table does not match metadata/header",
            missing_metadata=missing_metadata,
            not_implemented=not_implemented,
            missing_header=missing_header[:20],
            missing_header_count=len(missing_header),
            empty_cases=empty_cases,
            missing_body_calls=missing_body_calls,
            undeclared_body_calls=undeclared_body_calls,
            body_file_errors=body_file_errors,
            prologue_file_errors=prologue_file_errors,
            probe_count=len(cases))

    return _pass(
        check_name,
        probe_count=len(cases),
        primary_api_count=len(primary_names),
        support_api_count=len(all_names - primary_names),
        family_counts=_runtime_api_probe_family_counts(cases))


def _callback_api_work_package(name):
    """Return which callback implementation chunk owns an API name."""
    if name in CALLBACK_ATTRIBUTE_API_NAMES:
        return "chunk10a_attribute_callbacks"
    if name in LEGACY_ATTRIBUTE_API_NAMES:
        return "chunk10a_legacy_attribute_callbacks"
    if name in DEFERRED_CALLBACK_API_NAMES:
        return "chunk10b_callback_lifetime"
    return None


def _callback_api_coverage_audit(
        manifest, header, cases,
        check_name="installed_c_callback_api_coverage_audit",
        tolerated_legacy_names=None):
    """Report callback-owned C ABI APIs not covered by callback probes.

    The ordinary runtime audit treats callback APIs as deferred because
    callback failures can leave MPI objects or requests in undefined
    states.  This audit is the Phase 10 counterpart: every implemented
    callback-owned C API declared by the installed standard ABI header
    must be covered by a callback probe.  Unsupported optional callback
    families are still represented by probes, but those probes skip at
    run time with stable reasons when the installed implementation lacks
    the underlying feature.
    """
    if tolerated_legacy_names is None:
        tolerated_legacy_names = set()
    prototypes = _parse_c_header_prototypes(header)
    declared_names = set(prototypes)
    covered_names = _runtime_api_probe_api_names(cases)
    entries_by_name = {
        entry["name"]: entry for entry in manifest["apis"]
    }
    missing_by_package = {}
    legacy_not_declared = []
    unclassified_callback_apis = []
    covered = 0

    for entry in manifest["apis"]:
        if not entry["languages"]["c"]:
            continue
        name = entry["name"]
        package = _callback_api_work_package(name)
        if package is None:
            # Hard-coded ownership lists are intentional for reviewable
            # chunks, but they must not silently hide new callback APIs
            # that appear in docs/ metadata or the installed ABI header.
            # Treat any implemented, declared C API that metadata marks
            # as callback-owned as an audit failure until the API is
            # assigned to this chunk, a later callback chunk, or an
            # explicit skip category.
            if (entry["classification"] == CLASS_IMPLEMENTED and
                    name in declared_names and
                    (entry["callback"] or
                     _runtime_audit_work_package(entry) ==
                     "phase10_callback")):
                unclassified_callback_apis.append(name)
            continue
        if entry["classification"] != CLASS_IMPLEMENTED:
            if name in LEGACY_ATTRIBUTE_API_NAMES and name not in declared_names:
                legacy_not_declared.append(name)
            continue
        if name not in declared_names:
            if name in LEGACY_ATTRIBUTE_API_NAMES:
                legacy_not_declared.append(name)
            continue
        if name in covered_names:
            covered += 1
            continue
        if name in tolerated_legacy_names:
            continue
        missing_by_package.setdefault(package, []).append(name)

    # The main loop above is the single authority for recording missing
    # legacy attribute APIs: it already appends an implemented, declared,
    # uncovered, untolerated legacy name to its package exactly once, and
    # skips names that a callback probe covers.  legacy_declared is kept
    # only for the tolerated-legacy reporting fields below; re-adding it to
    # missing_by_package here would double-count uncovered names and flag
    # covered ones as missing.
    legacy_declared = sorted(
        name for name in LEGACY_ATTRIBUTE_API_NAMES
        if name in declared_names and
        entries_by_name.get(name, {}).get("classification") ==
        CLASS_IMPLEMENTED
    )

    counts = {
        package: len(names)
        for package, names in sorted(missing_by_package.items())
    }
    details = dict(
        covered=covered,
        legacy_not_declared=sorted(set(legacy_not_declared)),
        missing_by_package={
            package: names[:20]
            for package, names in sorted(missing_by_package.items())
        },
        missing_by_package_counts=counts,
        missing_count=sum(counts.values()),
        tolerated_legacy_declared=sorted(
            name for name in legacy_declared
            if name in tolerated_legacy_names
        ),
        tolerated_legacy_declared_count=len([
            name for name in legacy_declared
            if name in tolerated_legacy_names
        ]),
        unclassified_callback_apis=sorted(unclassified_callback_apis),
        unclassified_callback_api_count=len(unclassified_callback_apis),
    )
    if missing_by_package or unclassified_callback_apis:
        return _fail(
            check_name,
            "callback API coverage has undeferred gaps",
            **details)
    return _pass(check_name, **details)


def _prepare_installed_c_probe_body(srcdir, case, manifest, declared_names):
    """Expand generated C snippets inside one installed probe body.

    Long checked-in body snippets stay readable by using placeholders
    for generated constant lists and expected counts.  Substitution
    happens immediately before writing the temporary source, after the
    manifest has been built, configure-dependent Fortran gating is
    known, and the installed ABI header's declared Fortran datatype set
    has been parsed.
    """
    body = _probe_body_text(srcdir, case)
    replacements = {
        "@PREDEFINED_HANDLE_DECLS@": _predefined_handle_decls(manifest),
        "@PREDEFINED_HANDLE_CHECKS@": _predefined_handle_checks(manifest),
        "@PREDEFINED_HANDLE_GUARDS@": _predefined_handle_guards(manifest),
        "@FORTRAN_TYPE_CHECKS@": _fortran_datatype_checks(
            manifest, declared_names),
        "@ERROR_CLASS_CHECKS@": _error_class_checks(manifest),
        "@FORTRAN_TYPE_EXPECTED@": str(len(
            _declared_fortran_datatype_entries(manifest, declared_names))),
        "@ERROR_CLASS_EXPECTED@": str(len(_error_class_entries(manifest))),
    }
    for placeholder, generated in replacements.items():
        body = body.replace(placeholder, generated)
    return body


def _fortran_bindings_enabled(manifest):
    """Return whether any Fortran binding layer is enabled."""
    return any(
        item.get("enabled") is True
        for item in manifest["configuration"]["fortran"].values()
    )


def _optional_feature_info(manifest, feature):
    """Return configured optional feature state from the manifest."""
    return manifest["configuration"].get("optional_features", {}).get(
        feature, {
            "enabled": None,
            "source": "unknown",
        })


def _optional_feature_skip_reason(feature):
    """Return the stable skip reason for a disabled optional feature."""
    reasons = {
        "dynamic_process": SKIP_DYNAMIC_PROCESS_DISABLED,
        "mpi_io": SKIP_MPI_IO_SUPPORT_DISABLED,
        "mpit_events": SKIP_MPIT_EVENTS_DISABLED,
        "rma": SKIP_RMA_SUPPORT_DISABLED,
    }
    return reasons.get(feature, "optional_feature_disabled")


def _remove_c_comments(text):
    return re.sub(r"/\*.*?\*/", " ", text, flags=re.DOTALL)


def _copy_prototype_map(prototypes):
    """Return an isolated copy of a parsed prototype map.

    The per-name values are flat str->str dicts, so a per-entry shallow
    copy fully decouples the returned map from the shared cache.  This
    mirrors the defensive copy _parse_header_constant_names returns, so a
    caller cannot poison the memoized result for a later cross direction.
    """
    return {name: dict(entry) for name, entry in prototypes.items()}


def _parse_c_header_prototypes(header):
    """Parse MPI and PMPI C prototypes from an installed mpi.h.

    This parser is intentionally simple because it targets the generated
    ABI header format, not arbitrary C.  The caller enforces a high
    minimum prototype count so a future formatting change cannot reduce
    this to an empty or tiny parse that still passes downstream checks.
    """
    key = str(header)
    cached = _HEADER_PROTOTYPE_CACHE.get(key)
    if cached is not None:
        return _copy_prototype_map(cached)
    text = _remove_c_comments(_read_text(header))
    prototypes = {}
    pattern = re.compile(
        r"(?m)^\s*(?!typedef\b)"
        r"(?:(?:extern|OMPI_DECLSPEC)\s+)*"
        r"(?P<return_type>[A-Za-z_][A-Za-z0-9_\s]*\s*\**)\s+"
        r"(?P<name>(?:P)?MPI_\w+)\s*"
        r"\((?P<args>[^;{}]*)\)\s*;")
    for match in pattern.finditer(text):
        return_type = _normalize_c_signature_text(
            " ".join(match.group("return_type").split()))
        args = _normalize_c_signature_text(
            " ".join(match.group("args").split()))
        name = match.group("name")
        signature = "{0} {1}({2})".format(return_type, name, args)
        prototypes[name] = {
            "name": name,
            "return_type": return_type,
            "args": args,
            "signature": _normalize_c_signature_text(signature),
            "prototype": match.group(0).strip(),
        }
    _HEADER_PROTOTYPE_CACHE[key] = prototypes
    return _copy_prototype_map(prototypes)


def _normalize_c_signature_text(text):
    """Normalize C signature spelling for stable comparisons."""
    text = text.strip().rstrip(";")
    text = text.replace("char argv[]", "char *argv[]")
    text = re.sub(r"\s+", " ", text)
    text = re.sub(r"\s*\*\s*", "* ", text)
    text = re.sub(r"\s+([,\)])", r"\1", text)
    text = re.sub(r"\(\s+", "(", text)
    return text.strip()
