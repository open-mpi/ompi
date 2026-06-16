#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Metadata loading, feature detection, API/constant
classification, and manifest construction."""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    CLASS_IMPLEMENTED, CLASS_NOT_IMPLEMENTED, CLASS_NOT_IN_STANDARD_ABI,
    EXPECTED_API_COUNT, EXPECTED_CONSTANT_COUNT, EXPECTED_METADATA_VERSION,
    TEST_CALLBACK_DEFERRED, TEST_NOT_APPLICABLE, TEST_NOT_WRITTEN,
    _read_json)
from _abi_discovery import (
    _env_bool)


def _api_stem(api_key, api_name):
    name = api_key
    if name.startswith("mpi_"):
        name = name[4:]
    elif api_name.startswith("MPI_"):
        name = api_name[4:].lower()
    return name


def _api_family(name):
    """Return a coarse MPI API family used for test planning metadata."""
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
    """Return the default rank count needed for an API family."""
    if family in ("collective", "point_to_point"):
        return 2
    return 1


def _feature_requirement(family):
    """Return the optional Open MPI feature that gates an API family."""
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
    """Detect whether this build configured Open MPI standard ABI support."""
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
    """Read an Automake conditional value from top-level config.status.

    The ABI runner is usually invoked from test/mpi-abi via recursive
    make, but the conditionals we need live in the top build directory's
    config.status.  Treat an unreadable conditional as unknown, not
    disabled, so a VPATH or partial tree does not accidentally claim a
    configured feature state it did not actually observe.
    """
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
    """Discover which Open MPI Fortran binding layers were configured."""
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
    """Return configured optional MPI features that affect ABI coverage."""
    return {
        "dynamic_process": _detect_dynamic_process_support(),
        "mpi_io": _detect_mpi_io_support(builddir),
        "mpit_events": _detect_mpit_event_support(),
        "rma": _detect_rma_support(builddir),
    }


def _detect_dynamic_process_support():
    """Detect whether launch-sensitive dynamic process probes should run.

    MPI_Comm_spawn, MPI_Comm_connect/accept, and the name-service calls
    are standard MPI APIs, but many CI environments deliberately run with
    launchers or container restrictions that cannot service dynamic
    process management.  Open MPI does not expose a single configure
    conditional that captures that site policy, so the test runner treats
    dynamic process support as enabled unless the caller explicitly
    disables it with OMPI_ABI_TEST_DYNAMIC_PROCESS=0.  That gives
    restricted environments a stable SKIP instead of a hard FAIL while
    still running the probes by default on ordinary local installations.
    """
    override = _env_bool("OMPI_ABI_TEST_DYNAMIC_PROCESS")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_DYNAMIC_PROCESS",
        }
    return {
        "enabled": None,
        "source": "unknown",
    }


def _detect_mpi_io_support(builddir):
    """Detect whether the configured build includes MPI-IO support."""
    override = _env_bool("OMPI_ABI_TEST_MPI_IO")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_MPI_IO",
        }
    return _conditional_enabled(builddir, "OMPI_OMPIO_SUPPORT")


def _detect_mpit_event_support():
    """Detect whether MPI_T event callback probes should run.

    The MPI_T event APIs may be present in the ABI header even when the
    implementation has no registered event sources that can drive a
    callback.  The C probe performs the definitive runtime check and
    returns a stable skip code when no event registration can be
    allocated.  This override lets sites that know MPI_T events are not
    usable avoid the compile/run step entirely.
    """
    override = _env_bool("OMPI_ABI_TEST_MPIT_EVENTS")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_MPIT_EVENTS",
        }
    return {
        "enabled": None,
        "source": "unknown",
    }


def _detect_rma_support(builddir):
    """Detect whether the configured build includes an OSC component.

    RMA/window APIs require at least one OSC component at run time.  Open
    MPI's component Makefiles use DSO conditionals for install mode, so
    a false *_DSO conditional alone does not mean the component is
    disabled; a static build can still configure the component.  The
    generated config.status file is a better authority because it lists
    the component Makefiles that configure selected.
    """
    override = _env_bool("OMPI_ABI_TEST_RMA")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_RMA",
        }

    config_status = builddir / "config.status"
    if not config_status.exists():
        return {
            "enabled": None,
            "source": "unknown",
        }

    text = config_status.read_text(encoding="utf-8", errors="ignore")
    configured = sorted(set(
        match.group(1)
        for match in re.finditer(r"ompi/mca/osc/([^/]+)/Makefile", text)
        if match.group(1) != "base"
    ))
    if configured:
        return {
            "enabled": True,
            "source": str(config_status),
            "configured_components": configured,
        }
    if ("ompi/mca/osc/Makefile" not in text and
            "ompi/mca/osc/base/Makefile" not in text):
        return {
            "enabled": None,
            "source": str(config_status),
            "configured_components": configured,
        }
    return {
        "enabled": False,
        "source": str(config_status),
        "configured_components": configured,
    }


def _abi_metadata_version(abi):
    """Return the MPI ABI metadata version as MAJOR.MINOR."""
    mpi = abi.get("metadata", {}).get("mpi", {})
    version = mpi.get("version")
    subversion = mpi.get("subversion")
    if version is None or subversion is None:
        return None
    return "{0}.{1}".format(version, subversion)


def _source_exists(srcdir, stem):
    """Return whether an Open MPI C binding implementation exists."""
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
    if stem.startswith("t_"):
        tool_stem = stem[2:]
        candidates += (
            tool_dir / (tool_stem + "_abi_generated.c"),
            tool_dir / (tool_stem + ".c.in"),
            tool_dir / (tool_stem + ".c"),
        )
    return any(candidate.exists() for candidate in candidates)


def _classify_api(srcdir, api_key, api):
    """Classify one MPI API metadata entry for coverage accounting.

    This phase is a manifest-level inventory, not the final executable
    test suite.  Source existence is used as the conservative signal
    that Open MPI currently implements the C ABI binding.  Callback APIs
    are marked separately because many require caller-provided functions
    and cannot be honestly covered by a simple one-shot smoke probe.
    """
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
    """Classify one standard ABI constant metadata entry."""
    c_handle = constant.get("handle_types", {}).get("c", {})
    return {
        "kind": "constant",
        "key": key,
        "name": constant.get("name", key),
        "category": constant.get("category"),
        "abi_value": constant.get("abi_value"),
        "c_type": c_handle.get("type"),
        "datatypes": constant.get("datatypes", {}),
        "classification": CLASS_IMPLEMENTED,
        "test_status": TEST_NOT_WRITTEN,
        "skip_reason": None,
    }


def load_metadata(srcdir):
    """Load and validate the docs/ MPI API and ABI metadata files.

    docs/ is the authority for this test suite.  Version and entry-count
    checks are deliberately strict so a regenerated or repackaged
    metadata file cannot silently change test coverage.
    """
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
    """Build the full test manifest from metadata and configure output."""
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
