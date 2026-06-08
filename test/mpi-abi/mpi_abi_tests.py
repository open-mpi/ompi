#!/usr/bin/env python3
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""MPI standard ABI test manifest and runner.

This implementation provides the phase 1-6 infrastructure: metadata
loading, manifest generation, fast metadata checks, installed ABI smoke
tests, tool discovery, skip handling, and JSON/text reporting.  Later
phases add exhaustive generated compile, link, and runtime tests on top
of this runner.
"""

import argparse
import ast
import json
import os
from pathlib import Path
import platform
import re
import shlex
import shutil
import subprocess
import sys
import tempfile


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
SKIP_MPICH_TOOLS_UNAVAILABLE = "mpich_tools_unavailable"
SKIP_HEADER_UNAVAILABLE = "generated_standard_abi_header_unavailable"
SKIP_LINKAGE_INSPECTION_UNAVAILABLE = "linkage_inspection_unavailable"
SKIP_FORTRAN_BINDINGS_DISABLED = "fortran_bindings_disabled"
SKIP_FORTRAN_HELPERS_SHARED = "fortran_abi_helpers_shared_with_mpifh"

EXPECTED_METADATA_VERSION = "5.0"
EXPECTED_API_COUNT = 567
EXPECTED_CONSTANT_COUNT = 373
DEFAULT_COMMAND_TIMEOUT = 120

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

FORTRAN_ABI_HELPERS = (
    "abi_get_fortran_booleans",
    "abi_get_fortran_info",
    "abi_get_info",
    "abi_get_version",
    "abi_set_fortran_booleans",
    "abi_set_fortran_info",
)

INSTALLED_C_ABI_PROBES = (
    {
        "name": "abi_version",
        "rank_count": 1,
        "body": """
    int major = -1;
    int minor = -1;
    int ret = MPI_Abi_get_version(&major, &minor);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    if (MPI_ABI_VERSION != major) {
        return 2;
    }
    if (MPI_ABI_SUBVERSION != minor) {
        return 3;
    }
""",
    },
    {
        "name": "init_finalize",
        "rank_count": 1,
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 2;
    }
""",
    },
    {
        "name": "barrier_two_rank",
        "rank_count": 2,
        "body": """
    int size = 0;
    int rank = -1;
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    if (size != @EXPECTED_RANKS@ || rank < 0 || rank >= size) {
        return 4;
    }
    ret = MPI_Barrier(MPI_COMM_WORLD);
    if (MPI_SUCCESS != ret) {
        return 5;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 6;
    }
""",
    },
    {
        "name": "sendrecv_two_rank",
        "rank_count": 2,
        "body": """
    int size = 0;
    int rank = -1;
    int value = -1;
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    if (size != @EXPECTED_RANKS@ || rank < 0 || rank >= size) {
        return 4;
    }
    if (0 == rank) {
        value = 1234;
        ret = MPI_Send(&value, 1, MPI_INT, 1, 99, MPI_COMM_WORLD);
        if (MPI_SUCCESS != ret) {
            return 5;
        }
    } else if (1 == rank) {
        ret = MPI_Recv(&value, 1, MPI_INT, 0, 99,
                       MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != ret) {
            return 6;
        }
        if (1234 != value) {
            return 7;
        }
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 8;
    }
""",
    },
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


def _env_bool(name):
    value = os.environ.get(name)
    if value is None:
        return None
    return value.strip().lower() in ("1", "true", "yes", "on")


def _which(env_name, default_name):
    override = os.environ.get(env_name)
    if override:
        return override
    return shutil.which(default_name)


def _tool_available(path):
    if not path:
        return False
    if os.path.sep in path:
        return os.access(path, os.X_OK)
    return shutil.which(path) is not None


def _open_mpi_tools_available(tools):
    ompi_tools = tools["open_mpi"]
    return (
        _tool_available(ompi_tools["mpicc_abi"]) and
        _tool_available(ompi_tools["mpirun"])
    )


def _mpich_tools_available(tools):
    mpich_tools = tools["mpich"]
    return (
        _tool_available(mpich_tools["mpicc"]) and
        _tool_available(mpich_tools["mpirun"])
    )


def _api_stem(api_key, api_name):
    name = api_key
    if name.startswith("mpi_"):
        name = name[4:]
    elif api_name.startswith("MPI_"):
        name = api_name[4:].lower()
    return name


def _api_family(name):
    lower = name.lower()
    if lower.startswith("mpi_t_"):
        return "mpi_t"
    if lower.startswith("mpi_abi_"):
        return "abi"
    parts = lower.split("_")
    if len(parts) < 2:
        return "misc"
    if parts[1] in ("comm", "group", "type", "file", "win", "session"):
        return parts[1]
    if parts[1] in ("send", "recv", "isend", "irecv", "probe", "iprobe"):
        return "point_to_point"
    if parts[1] in ("allreduce", "allgather", "allgatherv", "alltoall"):
        return "collective"
    if parts[1] in ("barrier", "bcast", "gather", "gatherv"):
        return "collective"
    if parts[1] in ("reduce", "scan", "exscan"):
        return "collective"
    if parts[1] in ("put", "get", "accumulate", "rput", "rget"):
        return "rma"
    if parts[1] in ("init", "finalize", "initialized", "finalized"):
        return "init"
    if parts[1] in ("errhandler", "error"):
        return "error"
    return parts[1]


def _rank_requirement(family):
    if family in ("collective", "point_to_point"):
        return 2
    return 1


def _feature_requirement(family):
    if family == "file":
        return "mpi_io"
    if family == "mpi_t":
        return "mpi_t"
    if family == "win" or family == "rma":
        return "rma"
    if family == "session":
        return "sessions"
    return None


def _find_standard_abi_setting(srcdir, builddir):
    override = _env_bool("OMPI_ABI_TEST_STANDARD_ABI")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_STANDARD_ABI",
        }

    conditional = _conditional_enabled(builddir, "OMPI_STANDARD_ABI")
    if conditional["enabled"] is not None:
        return conditional

    return {
        "enabled": None,
        "source": "unknown",
    }


def _conditional_enabled(builddir, conditional):
    config_status = builddir / "config.status"
    if not config_status.exists():
        return {
            "enabled": None,
            "source": "unknown",
        }

    text = config_status.read_text(encoding="utf-8", errors="ignore")
    true_name = conditional + "_TRUE"
    false_name = conditional + "_FALSE"
    true_pattern = r'S\["' + re.escape(true_name) + r'"\]="([^"]*)"'
    false_pattern = r'S\["' + re.escape(false_name) + r'"\]="([^"]*)"'
    true_match = re.search(true_pattern, text)
    false_match = re.search(false_pattern, text)
    if true_match is not None:
        return {
            "enabled": true_match.group(1) == "",
            "source": str(config_status),
        }
    if false_match is not None:
        return {
            "enabled": false_match.group(1) != "",
            "source": str(config_status),
        }
    return {
        "enabled": None,
        "source": str(config_status),
    }


def _detect_fortran_support(builddir):
    conditionals = {
        "mpif.h": "OMPI_BUILD_FORTRAN_MPIFH_BINDINGS",
        "use mpi": "OMPI_BUILD_FORTRAN_USEMPI_BINDINGS",
        "use mpi_f08": "OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS",
    }
    detected = {}
    for language, conditional in conditionals.items():
        detected[language] = _conditional_enabled(builddir, conditional)
    return detected


def _detect_optional_features(builddir):
    # No configured optional MPI API-family feature currently maps
    # cleanly to a standard ABI unsupported_by_build classification.
    return {}


def _abi_metadata_version(abi):
    mpi = abi.get("metadata", {}).get("mpi", {})
    version = mpi.get("version")
    subversion = mpi.get("subversion")
    if version is None or subversion is None:
        return None
    return "{0}.{1}".format(version, subversion)


def _source_exists(srcdir, stem):
    c_dir = srcdir / "ompi" / "mpi" / "c"
    tool_dir = srcdir / "ompi" / "mpi" / "tool"
    candidates = (
        c_dir / (stem + "_abi_generated.c"),
        c_dir / (stem + "_abi.c"),
        c_dir / (stem + ".c.in"),
        tool_dir / (stem + "_abi_generated.c"),
        tool_dir / (stem + ".c.in"),
        tool_dir / (stem + ".c"),
    )
    return any(candidate.exists() for candidate in candidates)


def _classify_api(srcdir, api_key, api):
    attrs = api.get("attributes", {})
    name = api.get("name", api_key)
    stem = _api_stem(api_key, name)
    family = _api_family(name)
    c_expressible = bool(attrs.get("c_expressible"))
    f90_expressible = bool(attrs.get("f90_expressible"))
    f08_expressible = bool(attrs.get("f08_expressible"))
    mpif_expressible = bool(attrs.get("lis_expressible"))
    callback = bool(attrs.get("callback")) or any(
        param.get("func_type") for param in api.get("parameters", [])
    )

    if not any((c_expressible, f90_expressible, f08_expressible,
                mpif_expressible)):
        classification = CLASS_NOT_IN_STANDARD_ABI
        test_status = TEST_NOT_APPLICABLE
    elif _source_exists(srcdir, stem):
        classification = CLASS_IMPLEMENTED
        test_status = TEST_NOT_WRITTEN
    elif callback:
        classification = CLASS_NOT_IMPLEMENTED
        test_status = TEST_CALLBACK_DEFERRED
    else:
        classification = CLASS_NOT_IMPLEMENTED
        test_status = TEST_NOT_APPLICABLE

    skip_reason = None
    if classification != CLASS_IMPLEMENTED:
        skip_reason = classification

    return {
        "kind": "api",
        "key": api_key,
        "name": name,
        "stem": stem,
        "family": family,
        "classification": classification,
        "test_status": test_status,
        "skip_reason": skip_reason,
        "rank_count": _rank_requirement(family),
        "feature": _feature_requirement(family),
        "callback": callback,
        "requires_callback_test": callback,
        "languages": {
            "c": c_expressible,
            "mpif.h": (
                mpif_expressible and not bool(attrs.get("not_with_mpif"))
            ),
            "use mpi": f90_expressible,
            "use mpi_f08": f08_expressible,
        },
    }


def _classify_constant(key, constant):
    c_handle = constant.get("handle_types", {}).get("c", {})
    return {
        "kind": "constant",
        "key": key,
        "name": constant.get("name", key),
        "category": constant.get("category"),
        "abi_value": constant.get("abi_value"),
        "c_type": c_handle.get("type"),
        "classification": CLASS_IMPLEMENTED,
        "test_status": TEST_NOT_WRITTEN,
        "skip_reason": None,
    }


def load_metadata(srcdir):
    api_path = srcdir / "docs" / "mpi-standard-apis.json"
    abi_path = srcdir / "docs" / "mpi-standard-abi.json"
    api_resolved = api_path.resolve()
    abi_resolved = abi_path.resolve()
    apis = _read_json(api_path)
    abi = _read_json(abi_path)
    if not isinstance(apis, dict):
        raise RuntimeError("MPI API metadata must be a JSON object")
    if not isinstance(abi, dict):
        raise RuntimeError("MPI ABI metadata must be a JSON object")
    constants = abi.get("constants", {})
    if not isinstance(constants, dict):
        raise RuntimeError("MPI ABI constants metadata must be a JSON object")
    abi_version = _abi_metadata_version(abi)
    if abi_version != EXPECTED_METADATA_VERSION:
        raise RuntimeError(
            "unexpected MPI ABI metadata version: {0}".format(abi_version))
    if len(apis) != EXPECTED_API_COUNT:
        raise RuntimeError(
            "unexpected MPI API metadata entry count: {0} != {1}".format(
                len(apis), EXPECTED_API_COUNT))
    if len(constants) != EXPECTED_CONSTANT_COUNT:
        raise RuntimeError(
            "unexpected MPI ABI constant metadata entry count: {0} != {1}".
            format(len(constants), EXPECTED_CONSTANT_COUNT))
    return {
        "api_path": str(api_path),
        "abi_path": str(abi_path),
        "api_resolved_path": str(api_resolved),
        "abi_resolved_path": str(abi_resolved),
        "api_version": None,
        "api_version_source": "not present in API metadata",
        "abi_version": abi_version,
        "abi_version_source": "metadata.mpi",
        "expected_api_count": EXPECTED_API_COUNT,
        "expected_constant_count": EXPECTED_CONSTANT_COUNT,
        "apis": apis,
        "constants": constants,
        "abi_metadata": abi.get("metadata", {}),
    }


def build_manifest(srcdir, builddir):
    metadata = load_metadata(srcdir)
    standard_abi = _find_standard_abi_setting(srcdir, builddir)
    fortran = _detect_fortran_support(builddir)
    optional_features = _detect_optional_features(builddir)

    api_entries = [
        _classify_api(srcdir, key, api)
        for key, api in sorted(metadata["apis"].items())
    ]
    constant_entries = [
        _classify_constant(key, constant)
        for key, constant in sorted(metadata["constants"].items())
    ]

    return {
        "metadata": {
            "api_path": metadata["api_path"],
            "abi_path": metadata["abi_path"],
            "api_resolved_path": metadata["api_resolved_path"],
            "abi_resolved_path": metadata["abi_resolved_path"],
            "api_version": metadata["api_version"],
            "api_version_source": metadata["api_version_source"],
            "abi_version": metadata["abi_version"],
            "abi_version_source": metadata["abi_version_source"],
            "versions_match": (
                metadata["api_version"] == metadata["abi_version"]
                if metadata["api_version"] is not None else None
            ),
            "expected_api_count": metadata["expected_api_count"],
            "expected_constant_count": metadata["expected_constant_count"],
            "abi_metadata": metadata["abi_metadata"],
        },
        "configuration": {
            "standard_abi": standard_abi,
            "fortran": fortran,
            "optional_features": optional_features,
        },
        "apis": api_entries,
        "constants": constant_entries,
    }


def _check_result(name, result, details=None, reason=None):
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


def _manifest_sanity_checks(manifest):
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
    candidates = (
        builddir / "ompi" / "mpi" / "c" / "standard_abi" / "mpi.h",
        srcdir / "ompi" / "mpi" / "c" / "standard_abi" / "mpi.h",
    )
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return None


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
    if isinstance(node, ast.Expression):
        return _eval_integer_expression_node(node.body)
    if isinstance(node, ast.Constant) and type(node.value) is int:
        return node.value
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
    if isinstance(value, int):
        return value
    if isinstance(value, str):
        try:
            return int(value, 0)
        except ValueError:
            return None
    return None


def _parse_header_constants(path):
    constants = {}
    unparsed = {}
    define_re = re.compile(r"^\s*#define\s+(MPI\w+)\s+(.+?)\s*$")
    enum_re = re.compile(r"^\s*(MPI\w+)\s*=\s*([^,]+),")
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        match = define_re.match(line)
        if match is None:
            match = enum_re.match(line)
        if match is None:
            continue
        name, value = match.groups()
        integer = _extract_integer_constant(value)
        if integer is not None:
            constants[name] = integer
        else:
            unparsed[name] = value
    return constants, unparsed


def _header_constant_checks(manifest, srcdir, builddir):
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


def _abi_converter_checks(srcdir):
    c_dir = srcdir / "ompi" / "mpi" / "c"
    makefile = c_dir / "Makefile_abi.include"
    converter_header = c_dir / "abi_converters.h"
    converter_source = c_dir / "abi_converters.c"
    missing_files = []
    missing_patterns = []

    for path in (makefile, converter_header, converter_source):
        if not path.exists():
            missing_files.append(str(path))

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
        to_file = c_dir / (stem + "_toint_abi.c")
        from_file = c_dir / (stem + "_fromint_abi.c")
        to_source_name = to_file.name
        from_source_name = from_file.name
        to_symbol = "MPI_" + symbol + "_toint"
        from_symbol = "MPI_" + symbol + "_fromint"
        pto_symbol = "PMPI_" + symbol + "_toint"
        pfrom_symbol = "PMPI_" + symbol + "_fromint"

        for path in (to_file, from_file):
            if not path.exists():
                missing_files.append(str(path))

        for source_name in (to_source_name, from_source_name):
            if source_name not in makefile_text:
                missing_patterns.append({
                    "file": str(makefile),
                    "pattern": source_name,
                })

        if to_file.exists():
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

        if from_file.exists():
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
    return manifest["configuration"]["fortran"][language]["enabled"]


def _fortran_mpifh_helper_checks(srcdir, manifest):
    enabled = _fortran_enabled(manifest, "mpif.h")
    if enabled is False:
        return [_skip("fortran_mpifh_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="mpif.h")]

    base = srcdir / "ompi" / "mpi" / "fortran" / "mpif-h"
    makefile = base / "Makefile.am"
    prototypes = base / "prototypes_mpi.h"
    missing_files = []
    missing_patterns = []

    for path in (makefile, prototypes):
        if not path.exists():
            missing_files.append(str(path))

    makefile_text = _read_text(makefile) if makefile.exists() else ""
    prototypes_text = _read_text(prototypes) if prototypes.exists() else ""
    checked_sources = 0

    for helper in FORTRAN_ABI_HELPERS:
        c_name = helper + "_f.c"
        c_path = base / c_name
        helper_suffix = helper[4:]
        mixed_name = "MPI_Abi_" + helper_suffix
        lower_name = "mpi_abi_" + helper_suffix
        upper_name = lower_name.upper()
        internal_name = "ompi_" + helper + "_f"
        pmpi_name = "PMPI_Abi_" + helper_suffix

        if not c_path.exists():
            missing_files.append(str(c_path))
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


def _fortran_usempi_helper_checks(srcdir, manifest):
    enabled = _fortran_enabled(manifest, "use mpi")
    if enabled is False:
        return [_skip("fortran_usempi_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="use mpi")]

    makefile = (srcdir / "ompi" / "mpi" / "fortran" / "use-mpi" /
                "Makefile.am")
    if not makefile.exists():
        return [_fail(
            "fortran_usempi_abi_helpers",
            "use mpi Makefile is missing",
            configured=enabled,
            missing_files=[str(makefile)])]

    return [_skip(
        "fortran_usempi_abi_helpers",
        SKIP_FORTRAN_HELPERS_SHARED,
        configured=enabled,
        source=str(makefile),
        note="use mpi ABI helpers share the mpif.h helper entry points")]


def _fortran_f08_helper_checks(srcdir, manifest):
    enabled = _fortran_enabled(manifest, "use mpi_f08")
    if enabled is False:
        return [_skip("fortran_f08_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="use mpi_f08")]

    base = srcdir / "ompi" / "mpi" / "fortran" / "use-mpi-f08"
    prototype_file = base / "Makefile.prototype_files"
    interface_file = base / "mod" / "mpi-f08-interfaces-generated.h"
    missing_files = []
    missing_patterns = []

    for path in (prototype_file, interface_file):
        if not path.exists():
            missing_files.append(str(path))

    prototype_text = (
        _read_text(prototype_file) if prototype_file.exists() else ""
    )
    interface_text = (
        _read_text(interface_file) if interface_file.exists() else ""
    )
    checked_templates = 0

    for helper in FORTRAN_ABI_HELPERS:
        template_name = helper + ".c.in"
        template_path = base / template_name
        mpi_name = "MPI_Abi_" + helper[4:]
        pmpi_name = "PMPI_Abi_" + helper[4:]

        if not template_path.exists():
            missing_files.append(str(template_path))
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


def _fortran_helper_checks(srcdir, manifest):
    checks = []
    checks.extend(_fortran_mpifh_helper_checks(srcdir, manifest))
    checks.extend(_fortran_usempi_helper_checks(srcdir, manifest))
    checks.extend(_fortran_f08_helper_checks(srcdir, manifest))
    return checks


def run_fast_checks(manifest, srcdir, builddir):
    checks = []
    checks.extend(_manifest_sanity_checks(manifest))
    checks.extend(_header_constant_checks(manifest, srcdir, builddir))
    checks.extend(_abi_converter_checks(srcdir))
    checks.extend(_fortran_helper_checks(srcdir, manifest))
    return checks


def _count_by(entries, key):
    counts = {}
    for entry in entries:
        value = entry.get(key)
        counts[value] = counts.get(value, 0) + 1
    return counts


def _language_counts(entries):
    counts = {
        "c": 0,
        "mpif.h": 0,
        "use mpi": 0,
        "use mpi_f08": 0,
    }
    for entry in entries:
        for language, enabled in entry.get("languages", {}).items():
            if enabled:
                counts[language] += 1
    return counts


def _check_counts(checks):
    return _count_by(checks, "result")


def _command_timeout():
    return int(os.environ.get("OMPI_ABI_TEST_TIMEOUT",
                              str(DEFAULT_COMMAND_TIMEOUT)))


def _command_result(name, command, cwd, env, log_path):
    log_path.parent.mkdir(parents=True, exist_ok=True)
    timeout = _command_timeout()
    try:
        completed = subprocess.run(
            command,
            cwd=str(cwd),
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
            timeout=timeout)
        return_code = completed.returncode
        stdout = completed.stdout
        stderr = completed.stderr
        timed_out = False
    except subprocess.TimeoutExpired as exc:
        return_code = 124
        stdout = exc.stdout or ""
        stderr = exc.stderr or ""
        if isinstance(stdout, bytes):
            stdout = stdout.decode("utf-8", errors="replace")
        if isinstance(stderr, bytes):
            stderr = stderr.decode("utf-8", errors="replace")
        stderr += "\ncommand timed out after {0} seconds".format(timeout)
        timed_out = True
    except OSError as exc:
        return_code = 127
        stdout = ""
        stderr = str(exc)
        timed_out = False

    log = {
        "name": name,
        "command": command,
        "cwd": str(cwd),
        "returncode": return_code,
        "timeout": timeout,
        "timed_out": timed_out,
        "stdout": stdout,
        "stderr": stderr,
    }
    _write_json(log_path, log)
    return {
        "command": command,
        "returncode": return_code,
        "timeout": timeout,
        "timed_out": timed_out,
        "stdout": stdout,
        "stderr": stderr,
        "log": str(log_path),
    }


def _installed_test_env(tools):
    env = os.environ.copy()
    library_path = tools["paths"]["library"]
    loader_var = tools["paths"]["runtime_loader"]
    if library_path and loader_var:
        existing = env.get(loader_var)
        env[loader_var] = (
            library_path if not existing
            else library_path + os.pathsep + existing
        )
    return env


def _installed_test_dirs(outdir):
    base = outdir / "installed"
    dirs = {
        "base": base,
        "src": base / "src",
        "bin": base / "bin",
        "logs": base / "logs",
    }
    for path in dirs.values():
        path.mkdir(parents=True, exist_ok=True)
    return dirs


def _launcher_args(tools):
    args = tools["paths"]["launcher_args"]
    return shlex.split(args) if args else []


def _compile_overrides(tools):
    args = []
    include_path = tools["paths"]["include"]
    library_path = tools["paths"]["library"]
    if include_path:
        args.append("-I" + include_path)
    if library_path:
        args.append("-L" + library_path)
    return args


def _linkage_command(executable):
    system = platform.system()
    if system == "Darwin":
        tool = shutil.which("otool")
        if tool:
            return [tool, "-L", str(executable)]
    if system == "Linux":
        tool = shutil.which("readelf")
        if tool:
            return [tool, "-d", str(executable)]
    return None


def _verify_executable_libmpi_abi(executable, dirs, env, name):
    command = _linkage_command(executable)
    if command is None:
        return {
            "result": "SKIP",
            "skip_reason": SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
            "command": None,
            "returncode": 127,
            "log": None,
        }

    result = _command_result(
        "linkage_" + name,
        command,
        dirs["base"],
        env,
        dirs["logs"] / ("linkage_" + name + ".json"))
    output = result["stdout"] + result["stderr"]
    if result["returncode"] != 0:
        result["result"] = "FAIL"
        result["message"] = "executable linkage inspection failed"
    elif re.search(r"(?<![A-Za-z0-9_])libmpi_abi(?:[.][A-Za-z0-9_.-]+)?",
                   output) is None:
        result["result"] = "FAIL"
        result["message"] = "executable is not linked against libmpi_abi"
    else:
        result["result"] = "PASS"
        result["message"] = None
    return result


def _installed_wrapper_checks(tools, dirs):
    checks = []
    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    env = _installed_test_env(tools)
    compile_log = dirs["logs"] / "mpicc_abi_showme_compile.json"
    link_log = dirs["logs"] / "mpicc_abi_showme_link.json"
    compile_result = _command_result(
        "mpicc_abi_showme_compile",
        [mpicc_abi, "--showme:compile"],
        dirs["base"],
        env,
        compile_log)
    link_result = _command_result(
        "mpicc_abi_showme_link",
        [mpicc_abi, "--showme:link"],
        dirs["base"],
        env,
        link_log)

    if compile_result["returncode"] != 0:
        checks.append(_fail(
            "installed_standard_abi_wrapper_flags",
            "mpicc_abi --showme:compile failed",
            command=compile_result["command"],
            returncode=compile_result["returncode"],
            log=compile_result["log"]))
    elif "standard_abi" not in compile_result["stdout"]:
        checks.append(_fail(
            "installed_standard_abi_wrapper_flags",
            "mpicc_abi does not advertise standard ABI include path",
            command=compile_result["command"],
            stdout=compile_result["stdout"],
            log=compile_result["log"]))
    else:
        checks.append(_pass(
            "installed_standard_abi_wrapper_flags",
            command=compile_result["command"],
            stdout=compile_result["stdout"].strip(),
            log=compile_result["log"]))

    if link_result["returncode"] != 0:
        checks.append(_fail(
            "installed_libmpi_abi_wrapper_flags",
            "mpicc_abi --showme:link failed",
            command=link_result["command"],
            returncode=link_result["returncode"],
            log=link_result["log"]))
    elif re.search(r"(?<![A-Za-z0-9_])-lmpi_abi(?![A-Za-z0-9_])",
                   link_result["stdout"]) is None:
        checks.append(_fail(
            "installed_libmpi_abi_wrapper_flags",
            "mpicc_abi does not advertise libmpi_abi linkage",
            command=link_result["command"],
            stdout=link_result["stdout"],
            log=link_result["log"]))
    else:
        checks.append(_pass(
            "installed_libmpi_abi_wrapper_flags",
            command=link_result["command"],
            stdout=link_result["stdout"].strip(),
            log=link_result["log"]))

    return checks


def _c_probe_source(srcdir, body, rank_count):
    template = _read_text(srcdir / "test" / "mpi-abi" /
                          "templates" / "c_probe.c.in")
    body = body.replace("@EXPECTED_RANKS@", str(rank_count))
    return template.replace("@BODY@", body.rstrip())


def _installed_c_probe_checks(srcdir, tools, dirs):
    checks = []
    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    mpirun = tools["open_mpi"]["mpirun"]
    env = _installed_test_env(tools)
    launcher_args = _launcher_args(tools)
    compile_overrides = _compile_overrides(tools)

    for case in INSTALLED_C_ABI_PROBES:
        name = case["name"]
        rank_count = tools["rank_counts"]["np{0}".format(
            case["rank_count"])]
        source = dirs["src"] / (name + ".c")
        executable = dirs["bin"] / name
        _write_text(source, _c_probe_source(srcdir, case["body"], rank_count))

        compile_command = (
            [mpicc_abi] + compile_overrides +
            [str(source), "-o", str(executable)]
        )
        compile_result = _command_result(
            "compile_" + name,
            compile_command,
            dirs["base"],
            env,
            dirs["logs"] / ("compile_" + name + ".json"))
        if compile_result["returncode"] != 0:
            checks.append(_fail(
                "installed_c_probe_" + name,
                "installed C ABI probe compile failed",
                phase="compile",
                source=str(source),
                executable=str(executable),
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"]))
            continue

        linkage_result = _verify_executable_libmpi_abi(
            executable, dirs, env, name)
        if linkage_result["result"] == "SKIP":
            checks.append(_skip(
                "installed_c_probe_" + name,
                linkage_result["skip_reason"],
                phase="linkage",
                source=str(source),
                executable=str(executable),
                command=linkage_result["command"],
                returncode=linkage_result["returncode"],
                compile_log=compile_result["log"],
                linkage_log=linkage_result["log"]))
            continue

        if linkage_result["result"] != "PASS":
            checks.append(_fail(
                "installed_c_probe_" + name,
                linkage_result["message"],
                phase="linkage",
                source=str(source),
                executable=str(executable),
                command=linkage_result["command"],
                returncode=linkage_result["returncode"],
                compile_log=compile_result["log"],
                linkage_log=linkage_result["log"]))
            continue

        run_command = (
            [mpirun] + launcher_args +
            ["--np", str(rank_count), str(executable)]
        )
        run_result = _command_result(
            "run_" + name,
            run_command,
            dirs["base"],
            env,
            dirs["logs"] / ("run_" + name + ".json"))
        if run_result["returncode"] != 0:
            checks.append(_fail(
                "installed_c_probe_" + name,
                "installed C ABI probe runtime failed",
                phase="run",
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]))
            continue

        checks.append(_pass(
            "installed_c_probe_" + name,
            source=str(source),
            executable=str(executable),
            rank_count=rank_count,
            compile_command=compile_result["command"],
            run_command=run_result["command"],
            compile_log=compile_result["log"],
            linkage_command=linkage_result["command"],
            linkage_log=linkage_result["log"],
            run_log=run_result["log"]))

    return checks


def run_installed_checks(manifest, mode, srcdir, outdir, tools):
    del manifest
    if mode != "check-abi":
        return []
    dirs = _installed_test_dirs(outdir)
    checks = []
    checks.extend(_installed_wrapper_checks(tools, dirs))
    checks.extend(_installed_c_probe_checks(srcdir, tools, dirs))
    return checks


def _tool_info(mode):
    tools = {
        "open_mpi": {
            "mpicc_abi": _which("OMPI_ABI_TEST_MPICC_ABI", "mpicc_abi"),
            "mpirun": _which("OMPI_ABI_TEST_MPIRUN", "mpirun"),
        },
        "mpich": {
            "mpicc": _which("MPICH_ABI_TEST_MPICC", "mpicc"),
            "mpirun": _which("MPICH_ABI_TEST_MPIRUN", "mpirun"),
        },
        "rank_counts": {
            "np1": int(os.environ.get("OMPI_ABI_TEST_NP1", "1")),
            "np2": int(os.environ.get("OMPI_ABI_TEST_NP2", "2")),
        },
        "paths": {
            "include": os.environ.get("OMPI_ABI_TEST_INCLUDE_PATH"),
            "library": os.environ.get("OMPI_ABI_TEST_LIBRARY_PATH"),
            "launcher_args": os.environ.get("OMPI_ABI_TEST_LAUNCHER_ARGS"),
            "runtime_loader": _runtime_loader_var(),
        },
    }
    if mode != "check-abi-cross":
        tools.pop("mpich")
    return tools


def _symbol_diagnostics():
    system = platform.system()
    tools = {
        "nm": shutil.which("nm"),
    }
    if system == "Darwin":
        tools["otool"] = shutil.which("otool")
    elif system == "Linux":
        tools["readelf"] = shutil.which("readelf")
    return {
        "platform": system,
        "tools": tools,
    }


def _runtime_loader_var():
    system = platform.system()
    if system == "Darwin":
        return "DYLD_LIBRARY_PATH"
    if system == "Linux":
        return "LD_LIBRARY_PATH"
    return None


def build_report(manifest, mode, srcdir, builddir, outdir):
    api_entries = manifest["apis"]
    constant_entries = manifest["constants"]
    standard_abi = manifest["configuration"]["standard_abi"]
    classifications = _count_by(api_entries, "classification")
    test_status = _count_by(api_entries, "test_status")
    constant_classifications = _count_by(constant_entries, "classification")
    constant_test_status = _count_by(constant_entries, "test_status")

    skip_reason = None
    result = "PASS"
    tools = _tool_info(mode)
    fast_checks = []
    installed_checks = []

    if mode in ("coverage", "check-fast", "check-abi", "check-abi-cross"):
        if standard_abi["enabled"] is False:
            result = "SKIP"
            skip_reason = SKIP_STANDARD_ABI_DISABLED

    if result != "SKIP" and mode in ("coverage", "check-fast"):
        fast_checks = run_fast_checks(manifest, srcdir, builddir)
        if any(check["result"] == "FAIL" for check in fast_checks):
            result = "FAIL"

    if result != "SKIP" and mode == "check-abi":
        if not _open_mpi_tools_available(tools):
            result = "SKIP"
            skip_reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE
        else:
            installed_checks = run_installed_checks(
                manifest, mode, srcdir, outdir, tools)
            if any(check["result"] == "FAIL"
                   for check in installed_checks):
                result = "FAIL"

    if result != "SKIP" and mode == "check-abi-cross":
        if not _open_mpi_tools_available(tools):
            result = "SKIP"
            skip_reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE
        elif not _mpich_tools_available(tools):
            result = "SKIP"
            skip_reason = SKIP_MPICH_TOOLS_UNAVAILABLE

    return {
        "mode": mode,
        "result": result,
        "skip_reason": skip_reason,
        "srcdir": str(srcdir),
        "builddir": str(builddir),
        "tmpdir": os.environ.get("OMPI_ABI_TEST_TMPDIR")
                  or tempfile.gettempdir(),
        "tools": tools,
        "symbol_diagnostics": _symbol_diagnostics(),
        "fast_checks": fast_checks,
        "installed_checks": installed_checks,
        "summary": {
            "apis_total": len(api_entries),
            "api_classifications": classifications,
            "api_test_status": test_status,
            "constants_total": len(constant_entries),
            "constant_classifications": constant_classifications,
            "constant_test_status": constant_test_status,
            "fast_check_status": _check_counts(fast_checks),
            "installed_check_status": _check_counts(installed_checks),
            "language_coverage": _language_counts(api_entries),
        },
    }


def _summary_text(report):
    lines = []
    lines.append("MPI ABI test summary")
    lines.append("====================")
    lines.append("mode: {0}".format(report["mode"]))
    lines.append("result: {0}".format(report["result"]))
    if report["skip_reason"]:
        lines.append("skip_reason: {0}".format(report["skip_reason"]))
    lines.append("srcdir: {0}".format(report["srcdir"]))
    lines.append("builddir: {0}".format(report["builddir"]))
    lines.append("tmpdir: {0}".format(report["tmpdir"]))
    lines.append("")
    lines.append("API entries: {0}".format(report["summary"]["apis_total"]))
    for key, value in sorted(report["summary"]["api_classifications"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("API test status:")
    for key, value in sorted(report["summary"]["api_test_status"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("ABI constants: {0}".format(
        report["summary"]["constants_total"]))
    for key, value in sorted(
            report["summary"]["constant_classifications"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("ABI constant test status:")
    for key, value in sorted(
            report["summary"]["constant_test_status"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("Fast checks:")
    if report["fast_checks"]:
        for check in report["fast_checks"]:
            line = "  {0}: {1}".format(check["name"], check["result"])
            if check["skip_reason"]:
                line += " ({0})".format(check["skip_reason"])
            lines.append(line)
    else:
        lines.append("  none")
    lines.append("")
    lines.append("Installed checks:")
    if report["installed_checks"]:
        for check in report["installed_checks"]:
            line = "  {0}: {1}".format(check["name"], check["result"])
            if check["skip_reason"]:
                line += " ({0})".format(check["skip_reason"])
            lines.append(line)
    else:
        lines.append("  none")
    lines.append("")
    lines.append("Language coverage:")
    for key, value in sorted(report["summary"]["language_coverage"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("Tools:")
    lines.append(json.dumps(report["tools"], indent=2, sort_keys=True))
    lines.append("")
    lines.append("Symbol diagnostics:")
    lines.append(json.dumps(
        report["symbol_diagnostics"], indent=2, sort_keys=True))
    lines.append("")
    return "\n".join(lines)


def write_outputs(manifest, report, outdir):
    outdir.mkdir(parents=True, exist_ok=True)
    manifest_path = outdir / "abi-manifest.json"
    report_path = outdir / "abi-report.json"
    summary_path = outdir / "abi-summary.txt"
    _write_json(manifest_path, manifest)
    _write_json(report_path, report)
    _write_text(summary_path, _summary_text(report))
    return manifest_path, report_path, summary_path


def command(args):
    srcdir = Path(args.srcdir).resolve()
    builddir = Path(args.builddir).resolve()
    outdir = (
        Path(args.outdir).resolve() if args.outdir else builddir / ".mpi-abi"
    )
    manifest = build_manifest(srcdir, builddir)
    mode = args.mode

    if mode == "manifest":
        outdir.mkdir(parents=True, exist_ok=True)
        path = outdir / "abi-manifest.json"
        _write_json(path, manifest)
        print("Wrote {0}".format(path))
        return 0

    report = build_report(manifest, mode, srcdir, builddir, outdir)
    _, report_path, summary_path = write_outputs(manifest, report, outdir)
    print(_summary_text(report), end="")
    print("Wrote {0}".format(report_path))
    print("Wrote {0}".format(summary_path))

    if args.complete_gate and report["result"] != "SKIP":
        api_not_written = report["summary"]["api_test_status"].get(
            TEST_NOT_WRITTEN, 0)
        constant_not_written = report["summary"][
            "constant_test_status"].get(TEST_NOT_WRITTEN, 0)
        not_written = api_not_written + constant_not_written
        if not_written:
            print(
                "ERROR: completion gate found {0} unwritten tests "
                "({1} APIs, {2} constants)".format(
                    not_written, api_not_written, constant_not_written),
                file=sys.stderr)
            return 1

    if report["result"] == "FAIL":
        return 1

    return 0


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--srcdir", default=Path(__file__).parents[2],
                        help="top source directory")
    parser.add_argument("--builddir", default=os.getcwd(),
                        help="top build directory")
    parser.add_argument("--outdir",
                        help="output directory for generated reports")
    parser.add_argument("--complete-gate", action="store_true",
                        help="fail when implemented ABI entries lack tests")
    parser.add_argument("mode", choices=(
        "manifest",
        "coverage",
        "check-fast",
        "check-abi",
        "check-abi-cross",
    ))
    args = parser.parse_args(argv)
    try:
        return command(args)
    except Exception as exc:
        print("ERROR: {0}".format(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
