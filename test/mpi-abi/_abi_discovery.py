#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""MPI installation discovery, tool selection, and MPI Forum
ABI suitability checks."""

import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    _command_timeout, _read_text)


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
        _tool_available(ompi_tools["mpirun"]) and
        ompi_tools.get("identity") == "open_mpi" and
        ompi_tools.get("mpi_forum_abi_available") is True
    )


def _open_mpi_tools_present(tools):
    """Return whether a usable-looking Open MPI wrapper and launcher exist."""
    ompi_tools = tools["open_mpi"]
    return (
        _tool_available(ompi_tools["mpicc_abi"]) and
        _tool_available(ompi_tools["mpirun"])
    )


def _open_mpi_tool_override_requested():
    """Return whether the operator explicitly selected Open MPI tools."""
    return (
        _tool_override_used("OMPI_ABI_TEST_MPICC_ABI") or
        _tool_override_used("OMPI_ABI_TEST_MPIRUN")
    )


def _mpich_tools_available(tools):
    mpich_tools = tools["mpich"]
    return (
        _tool_available(mpich_tools["mpicc"]) and
        _tool_available(mpich_tools["mpirun"]) and
        mpich_tools.get("identity") == "mpich" and
        mpich_tools.get("mpi_forum_abi_available") is True
    )


def _tool_override_used(env_name):
    """Return whether a tool/path override was explicitly supplied."""
    return bool(os.environ.get(env_name))


def _quick_command_output(command):
    """Run a short discovery command without creating a report log.

    Tool availability checks happen before the cross scratch directories
    exist.  They need only a conservative identity signal, so this helper
    captures a bounded amount of output and treats launch errors as
    ordinary unavailable-tool evidence.
    """
    try:
        completed = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
            timeout=_command_timeout())
    except (OSError, subprocess.TimeoutExpired):
        return ""
    if completed.returncode != 0:
        return ""
    return (completed.stdout or "") + (completed.stderr or "")


def _executable_path(path):
    """Return an absolute executable path string, or None.

    Do not resolve symlinks here.  MPI compiler wrappers commonly use
    `argv[0]` to decide which wrapper-data file to load; resolving
    Open MPI's `mpicc_abi` symlink to the shared `opal_wrapper`
    executable changes that behavior and makes discovery fail.
    """
    if not path:
        return None
    candidate = Path(path)
    if not candidate.is_absolute() and os.path.sep not in path:
        found = shutil.which(path)
        if found is None:
            return None
        candidate = Path(found)
    if not candidate.exists() or not os.access(str(candidate), os.X_OK):
        return None
    return os.path.abspath(str(candidate))


def _path_executable(directory, name):
    """Return directory/name when it exists and is executable."""
    path = Path(directory) / name
    if path.exists() and os.access(str(path), os.X_OK):
        return os.path.abspath(str(path))
    return None


def _path_dirs_with_overrides(*env_names):
    """Return PATH directories plus directories containing overrides."""
    dirs = []
    for value in os.environ.get("PATH", "").split(os.pathsep):
        if value and value not in dirs:
            dirs.append(value)
    for env_name in env_names:
        value = os.environ.get(env_name)
        if not value:
            continue
        path = Path(value)
        directory = os.path.abspath(str(path.parent or Path(".")))
        if directory not in dirs:
            dirs.insert(0, directory)
    return dirs


def _all_named_executables(names, *override_envs):
    """Find all matching executable names on PATH and in override dirs."""
    found = []
    seen = set()
    for directory in _path_dirs_with_overrides(*override_envs):
        for name in names:
            path = _path_executable(directory, name)
            if path is not None and path not in seen:
                found.append(path)
                seen.add(path)
    return found


def _tool_prefix(path):
    """Infer an installation prefix from a bin/ tool path."""
    if path is None:
        return None
    candidate = Path(os.path.abspath(path))
    if candidate.parent.name == "bin":
        return str(candidate.parent.parent)
    return str(candidate.parent)


def _tool_bin_dir(path):
    """Return the bin directory for a tool path."""
    return str(Path(os.path.abspath(path)).parent) if path else None


def _same_tool_directory(left, right):
    """Return whether two tool paths live in the same directory."""
    if not left or not right:
        return False
    return Path(os.path.abspath(left)).parent == Path(os.path.abspath(right)).parent


def _first_line(text):
    """Return the first non-empty output line from a command."""
    for line in text.splitlines():
        if line.strip():
            return line.strip()
    return ""


def _command_words(output):
    """Shell-split wrapper output for include/lib flag extraction."""
    try:
        return shlex.split(output)
    except ValueError:
        return output.split()


def _flag_dirs_from_words(words, prefix):
    """Return directories from -I/-L wrapper flags."""
    dirs = []
    i = 0
    while i < len(words):
        word = words[i]
        value = None
        if word == prefix and i + 1 < len(words):
            value = words[i + 1]
            i += 1
        elif word.startswith(prefix) and len(word) > len(prefix):
            value = word[len(prefix):]
        if value and value not in dirs:
            dirs.append(value)
        i += 1
    return dirs


def _add_unique_path(paths, value, prepend=False):
    """Add a path string once, preserving stable path-search order."""
    if not value:
        return
    path = str(value)
    if path in paths:
        return
    if prepend:
        paths.insert(0, path)
    else:
        paths.append(path)


def _words_define_macro(words, macro):
    """Return whether compiler-wrapper words define a preprocessor macro."""
    i = 0
    while i < len(words):
        word = words[i]
        if word == "-D" and i + 1 < len(words):
            if words[i + 1] == macro or words[i + 1].startswith(macro + "="):
                return True
            i += 1
        elif word == "-D" + macro or word.startswith("-D" + macro + "="):
            return True
        i += 1
    return False


def _words_link_library(words, library):
    """Return whether wrapper words link the requested library name."""
    link_flag = "-l" + library
    library_pattern = re.compile(
        r"(?:^|[/\\])lib" + re.escape(library) +
        r"(?:[.][A-Za-z0-9_.-]+)?$")
    for word in words:
        if word == link_flag or library_pattern.search(word):
            return True
    return False


def _library_search_dirs(prefix, library_dirs):
    """Return likely library directories for wrapper and prefix evidence."""
    dirs = []
    for directory in library_dirs:
        if directory and directory not in dirs:
            dirs.append(directory)
    if prefix:
        for relative in ("lib", "lib64"):
            directory = str(Path(prefix) / relative)
            if directory not in dirs:
                dirs.append(directory)
    return dirs


def _library_artifact_exists(prefix, library_dirs, library):
    """Return whether an installed library artifact is present.

    Wrapper link flags prove what a compiler intends to request.  This
    filesystem check gives the failure report a concrete reason when a
    wrapper advertises MPI Forum ABI support but the matching installed
    library is absent or installed outside the searched library paths.
    """
    for directory in _library_search_dirs(prefix, library_dirs):
        path = Path(directory)
        if not path.is_dir():
            continue
        if any(path.glob("lib" + library + ".*")):
            return True
        for suffix in (".a", ".la"):
            if (path / ("lib" + library + suffix)).exists():
                return True
    return False


def _abi_suitability(spec, explicit_override, identity_confirmed,
                     header_available, wrapper_words, prefix, library_dirs):
    """Apply common MPI Forum ABI suitability checks.

    Open MPI and MPICH expose different wrapper query interfaces and
    different identity markers, but the final ABI-test suitability
    contract is the same shape: an implementation identity must be
    confirmed unless an explicit operator override asserts it, required
    ABI headers must be visible through the selected include paths,
    wrappers must advertise required ABI compile/link intent, and the
    corresponding ABI libraries must exist in the selected library
    paths.
    """
    reasons = []
    macros = {}
    link_libraries = {}
    installed_libraries = {}

    if not explicit_override and not identity_confirmed:
        reasons.append(spec["identity_reason"])
    if not header_available:
        reasons.append(spec["header_reason"])

    for macro in spec.get("required_macros", ()):
        macros[macro] = _words_define_macro(wrapper_words, macro)
        if not macros[macro]:
            reasons.append("wrapper_does_not_define_" + macro)

    for library in spec.get("required_libraries", ()):
        link_libraries[library] = _words_link_library(wrapper_words, library)
        if not link_libraries[library]:
            reasons.append("wrapper_does_not_link_lib" + library)

    for library in spec.get("required_libraries", ()):
        installed_libraries[library] = _library_artifact_exists(
            prefix, library_dirs, library)
        if not installed_libraries[library]:
            reasons.append("missing_lib" + library)

    return {
        "available": not reasons,
        "unsuitable_reasons": reasons,
        "macros": macros,
        "link_libraries": link_libraries,
        "installed_libraries": installed_libraries,
    }


def _read_header_markers(header):
    """Return standard implementation markers from one mpi.h."""
    markers = []
    if header is None or not Path(header).exists():
        return markers
    text = _read_text(Path(header))
    marker_patterns = (
        ("standard_abi", r"^\s*#define\s+MPI_H_ABI\b"),
        ("open_mpi", r"^\s*#define\s+OMPI_MPI_H\b"),
        ("mpich", r"^\s*#define\s+MPICH_(?:VERSION|NUMVERSION|NAME)\b"),
    )
    for name, pattern in marker_patterns:
        if re.search(pattern, text, re.MULTILINE):
            markers.append(name)
    return markers


def _mpi_h_candidates(prefix, include_dirs):
    """Return candidate mpi.h paths for prefix and wrapper include dirs."""
    candidates = []
    for directory in include_dirs:
        path = str((Path(directory) / "mpi.h").resolve())
        if path not in candidates:
            candidates.append(path)
    if prefix:
        for relative in ("include/standard_abi/mpi.h", "include/mpi.h"):
            path = str((Path(prefix) / relative).resolve())
            if path not in candidates:
                candidates.append(path)
    return candidates


def _header_evidence(prefix, include_dirs):
    """Return mpi.h marker evidence for candidate include directories."""
    evidence = []
    for header in _mpi_h_candidates(prefix, include_dirs):
        markers = _read_header_markers(header)
        if markers:
            evidence.append({
                "header": header,
                "markers": markers,
            })
    return evidence


def _has_header_marker(header_evidence, marker):
    """Return whether any discovered mpi.h has a requested marker."""
    return any(marker in item["markers"] for item in header_evidence)


def _mpi_abi_header_candidates(prefix, include_dirs):
    """Return candidate MPICH MPI Forum ABI header paths."""
    candidates = []
    for directory in include_dirs:
        path = str((Path(directory) / "mpi_abi.h").resolve())
        if path not in candidates:
            candidates.append(path)
    if prefix:
        path = str((Path(prefix) / "include" / "mpi_abi.h").resolve())
        if path not in candidates:
            candidates.append(path)
    return candidates


def _read_mpi_abi_header_markers(header):
    """Return MPI Forum ABI markers from an MPICH mpi_abi.h candidate."""
    markers = []
    if header is None or not Path(header).exists():
        return markers
    text = _read_text(Path(header))
    marker_patterns = (
        ("mpi_h_abi", r"^\s*#define\s+MPI_H_ABI\b"),
        ("mpi_abi_version", r"^\s*#define\s+MPI_ABI_VERSION\b"),
        ("mpi_abi_subversion", r"^\s*#define\s+MPI_ABI_SUBVERSION\b"),
    )
    for name, pattern in marker_patterns:
        if re.search(pattern, text, re.MULTILINE):
            markers.append(name)
    return markers


def _mpi_abi_header_evidence(prefix, include_dirs):
    """Return MPI Forum ABI header evidence for MPICH candidates."""
    evidence = []
    for header in _mpi_abi_header_candidates(prefix, include_dirs):
        markers = _read_mpi_abi_header_markers(header)
        if markers:
            evidence.append({
                "header": header,
                "markers": markers,
            })
    return evidence


def _has_mpi_forum_abi_header(header_evidence):
    """Return whether MPICH's installed mpi_abi.h has ABI version markers."""
    required = {"mpi_h_abi", "mpi_abi_version", "mpi_abi_subversion"}
    return any(required.issubset(set(item["markers"]))
               for item in header_evidence)


def _open_mpi_identity(explicit_override, is_open_mpi, links_mpi_abi):
    """Return the implementation identity for Open MPI ABI evidence."""
    if links_mpi_abi and (explicit_override or is_open_mpi):
        return "open_mpi"
    return "unknown"


def _open_mpi_candidate(mpicc_abi, mpirun=None, mpirun_override=False,
                        explicit_override=False, include_override=None,
                        library_override=None):
    """Classify one candidate Open MPI ABI installation."""
    mpicc_abi = _executable_path(mpicc_abi)
    if mpicc_abi is None:
        return None
    bin_dir = _tool_bin_dir(mpicc_abi)
    prefix = _tool_prefix(mpicc_abi)
    if mpirun_override:
        mpirun = _executable_path(mpirun)
    else:
        mpirun = (_executable_path(mpirun) or
                  _path_executable(bin_dir, "mpirun") or
                  _path_executable(bin_dir, "mpiexec"))
    ompi_info = _path_executable(bin_dir, "ompi_info")
    outputs = {
        "mpicc_abi_showme_version": _quick_command_output(
            [mpicc_abi, "--showme:version"]),
        "mpicc_abi_showme_compile": _quick_command_output(
            [mpicc_abi, "--showme:compile"]),
        "mpicc_abi_showme_link": _quick_command_output(
            [mpicc_abi, "--showme:link"]),
        "mpicc_abi_showme_incdirs": _quick_command_output(
            [mpicc_abi, "--showme:incdirs"]),
        "mpicc_abi_showme_libdirs": _quick_command_output(
            [mpicc_abi, "--showme:libdirs"]),
        "mpicc_abi_showme_libs": _quick_command_output(
            [mpicc_abi, "--showme:libs"]),
        "mpirun_version": (
            _quick_command_output([mpirun, "--version"]) if mpirun else ""
        ),
        "ompi_info_version": (
            _quick_command_output([ompi_info, "--version"])
            if ompi_info else ""
        ),
    }
    compile_words = _command_words(outputs["mpicc_abi_showme_compile"])
    include_dirs = []
    _add_unique_path(include_dirs, include_override, prepend=True)
    for item in outputs["mpicc_abi_showme_incdirs"].split():
        _add_unique_path(include_dirs, item)
    for directory in _flag_dirs_from_words(compile_words, "-I"):
        _add_unique_path(include_dirs, directory)
    if prefix:
        for directory in (str(Path(prefix) / "include" / "standard_abi"),
                          str(Path(prefix) / "include")):
            _add_unique_path(include_dirs, directory)

    link_words = _command_words(outputs["mpicc_abi_showme_link"])
    library_dirs = []
    _add_unique_path(library_dirs, library_override, prepend=True)
    for item in outputs["mpicc_abi_showme_libdirs"].split():
        _add_unique_path(library_dirs, item)
    for directory in _flag_dirs_from_words(link_words, "-L"):
        _add_unique_path(library_dirs, directory)

    headers = _header_evidence(prefix, include_dirs)
    combined = "\n".join(outputs.values())
    is_open_mpi = (
        re.search(r"\bOpen MPI\b", combined, re.IGNORECASE) is not None or
        _has_header_marker(headers, "open_mpi")
    )
    has_standard_abi = _has_header_marker(headers, "standard_abi")
    has_mpich_header = _has_header_marker(headers, "mpich")
    link_probe_words = []
    link_probe_words.extend(link_words)
    link_probe_words.extend(_command_words(outputs["mpicc_abi_showme_libs"]))
    suitability = _abi_suitability(
        {
            "identity_reason": "wrapper_not_identified_as_open_mpi",
            "header_reason": "missing_standard_abi_header",
            "required_libraries": ("mpi_abi",),
            "required_macros": (),
        },
        explicit_override,
        is_open_mpi,
        has_standard_abi,
        link_probe_words,
        prefix,
        library_dirs)
    links_mpi_abi = suitability["link_libraries"]["mpi_abi"]
    identity = _open_mpi_identity(
        explicit_override, is_open_mpi, links_mpi_abi)
    mpi_forum_abi_available = suitability["available"]
    return {
        "implementation": identity,
        "candidate_kind": "open_mpi",
        "prefix": prefix,
        "bin_dir": bin_dir,
        "valid": (
            identity == "open_mpi" and
            mpirun is not None and
            mpi_forum_abi_available
        ),
        "tools": {
            "mpicc_abi": mpicc_abi,
            "mpirun": mpirun,
            "ompi_info": ompi_info,
        },
        "paths": {
            "include_dirs": include_dirs,
            "library_dirs": library_dirs,
        },
        "evidence": {
            "headers": headers,
            "outputs": {
                key: _first_line(value) for key, value in outputs.items()
            },
            "is_open_mpi": is_open_mpi,
            "has_standard_abi_header": has_standard_abi,
            "has_mpich_header": has_mpich_header,
            "links_mpi_abi": links_mpi_abi,
            "has_mpi_abi_library": (
                suitability["installed_libraries"]["mpi_abi"]
            ),
            "explicit_override": explicit_override,
            "mpi_forum_abi_available": mpi_forum_abi_available,
            "unsuitable_reasons": suitability["unsuitable_reasons"],
        },
    }


def _mpich_identity(explicit_override, mentions_open_mpi, mentions_mpich,
                    has_mpich_header):
    """Return the implementation identity for MPICH candidate evidence."""
    if mentions_open_mpi:
        return "unknown"
    if explicit_override or mentions_mpich or has_mpich_header:
        return "mpich"
    return "unknown"


def _mpich_device_from_version(output):
    """Return the MPICH device reported by mpichversion, if available."""
    match = re.search(r"^MPICH Device:\s*(\S+)", output, re.MULTILINE)
    return match.group(1) if match else None


def _mpich_candidate(mpicc, mpirun=None, mpirun_override=False,
                     explicit_override=False, include_override=None,
                     library_override=None):
    """Classify one candidate MPICH MPI Forum ABI installation."""
    mpicc = _executable_path(mpicc)
    if mpicc is None:
        return None
    bin_dir = _tool_bin_dir(mpicc)
    prefix = _tool_prefix(mpicc)
    if mpirun_override:
        mpirun = _executable_path(mpirun)
    else:
        mpirun = (_executable_path(mpirun) or
                  _path_executable(bin_dir, "mpirun") or
                  _path_executable(bin_dir, "mpiexec"))
    mpichversion = _path_executable(bin_dir, "mpichversion")
    outputs = {
        "mpicc_show": _quick_command_output([mpicc, "-show"]),
        "mpicc_compile_info": "",
        "mpicc_link_info": "",
        "mpicc_version": _quick_command_output([mpicc, "--version"]),
        "mpirun_version": (
            _quick_command_output([mpirun, "--version"]) if mpirun else ""
        ),
        "mpichversion": (
            _quick_command_output([mpichversion]) if mpichversion else ""
        ),
    }
    wrapper_words = []
    wrapper_words.extend(_command_words(outputs["mpicc_show"]))
    include_dirs = []
    _add_unique_path(include_dirs, include_override, prepend=True)
    for directory in _flag_dirs_from_words(wrapper_words, "-I"):
        _add_unique_path(include_dirs, directory)
    if prefix:
        _add_unique_path(include_dirs, str(Path(prefix) / "include"))
    library_dirs = []
    _add_unique_path(library_dirs, library_override, prepend=True)
    for directory in _flag_dirs_from_words(wrapper_words, "-L"):
        _add_unique_path(library_dirs, directory)
    headers = _header_evidence(prefix, include_dirs)
    mpi_abi_headers = _mpi_abi_header_evidence(prefix, include_dirs)
    combined = "\n".join(outputs.values())
    mentions_open_mpi = re.search(r"\b(Open MPI|OpenRTE|PRRTE)\b",
                                  combined, re.IGNORECASE) is not None
    mentions_mpich = re.search(r"\bMPICH\b", combined,
                               re.IGNORECASE) is not None
    mpich_device = _mpich_device_from_version(outputs["mpichversion"])
    has_mpich_header = _has_header_marker(headers, "mpich")
    identity = _mpich_identity(
        explicit_override, mentions_open_mpi, mentions_mpich, has_mpich_header)
    has_mpi_forum_abi_header = _has_mpi_forum_abi_header(mpi_abi_headers)
    suitability = _abi_suitability(
        {
            "identity_reason": "wrapper_not_identified_as_mpich",
            "header_reason": "missing_mpi_forum_abi_header",
            "required_libraries": ("mpi_abi",),
            "required_macros": ("MPI_ABI",),
        },
        explicit_override,
        mentions_mpich or has_mpich_header,
        has_mpi_forum_abi_header,
        wrapper_words,
        prefix,
        library_dirs)
    if mentions_open_mpi:
        suitability["unsuitable_reasons"].append(
            "candidate_identified_as_open_mpi")
        suitability["available"] = False
    mpi_forum_abi_available = suitability["available"]
    return {
        "implementation": identity,
        "candidate_kind": "mpich",
        "prefix": prefix,
        "bin_dir": bin_dir,
        "valid": (
            identity == "mpich" and
            mpirun is not None and
            mpi_forum_abi_available
        ),
        "tools": {
            "mpicc": mpicc,
            "mpirun": mpirun,
            "mpichversion": mpichversion,
        },
        "paths": {
            "include_dirs": include_dirs,
            "library_dirs": library_dirs,
        },
        "evidence": {
            "headers": headers,
            "mpi_abi_headers": mpi_abi_headers,
            "outputs": {
                key: _first_line(value) for key, value in outputs.items()
            },
            "mentions_mpich": mentions_mpich,
            "mentions_open_mpi": mentions_open_mpi,
            "mpich_device": mpich_device,
            "has_mpich_header": has_mpich_header,
            "explicit_override": explicit_override,
            "defines_mpi_abi": suitability["macros"]["MPI_ABI"],
            "links_mpi_abi": suitability["link_libraries"]["mpi_abi"],
            "has_mpi_forum_abi_header": has_mpi_forum_abi_header,
            "has_mpi_abi_library": (
                suitability["installed_libraries"]["mpi_abi"]
            ),
            "mpi_forum_abi_available": mpi_forum_abi_available,
            "unsuitable_reasons": suitability["unsuitable_reasons"],
        },
    }


def _dedupe_install_candidates(candidates, include_invalid=False):
    """Remove duplicate installation candidates while preserving order."""
    deduped = []
    seen = set()
    for candidate in candidates:
        if candidate is None:
            continue
        if not include_invalid and not candidate.get("valid"):
            continue
        key = (
            candidate.get("candidate_kind"),
            candidate.get("tools", {}).get("mpicc_abi") or
            candidate.get("tools", {}).get("mpicc"),
        )
        if key not in seen:
            deduped.append(candidate)
            seen.add(key)
    return deduped


def _mpich_report_candidates(candidates, include_invalid=False):
    """Return MPICH candidates worth reporting.

    When no valid MPICH MPI Forum ABI install is found, keep invalid
    candidates only if they are plausibly MPICH or came from an explicit
    operator override.  This preserves actionable diagnostics for a
    non-ABI MPICH install without showing rejected Open MPI wrappers as
    MPICH candidates.
    """
    if not include_invalid:
        return _dedupe_install_candidates(candidates)
    reportable = []
    for candidate in candidates:
        if candidate is None:
            continue
        evidence = candidate.get("evidence", {})
        if (candidate.get("valid") or
                candidate.get("implementation") == "mpich" or
                evidence.get("explicit_override")):
            reportable.append(candidate)
    return _dedupe_install_candidates(reportable, include_invalid=True)


def _open_mpi_report_candidates(candidates, include_invalid=False):
    """Return Open MPI candidates worth reporting.

    Keep rejected Open MPI-looking ABI wrappers so a classification
    failure reports the exact wrapper and reason.  Do not retain MPICH
    ABI wrappers that were seen only because both implementations use an
    `mpicc_abi` executable name.
    """
    if not include_invalid:
        return _dedupe_install_candidates(candidates)
    reportable = []
    for candidate in candidates:
        if candidate is None:
            continue
        evidence = candidate.get("evidence", {})
        if (candidate.get("valid") or
                evidence.get("explicit_override") or
                evidence.get("is_open_mpi") or
                evidence.get("has_standard_abi_header") or
                (evidence.get("links_mpi_abi") and
                 not evidence.get("has_mpich_header"))):
            reportable.append(candidate)
    return _dedupe_install_candidates(reportable, include_invalid=True)


def _has_valid_candidate(candidates):
    """Return whether a candidate list contains a usable installation."""
    return any(candidate is not None and candidate.get("valid")
               for candidate in candidates)


def _discover_mpi_installations(include_open_mpi=True, include_mpich=True):
    """Discover and classify MPI installations visible to the runner."""
    open_mpi_candidates = []
    mpich_candidates = []
    include_invalid_open_mpi = False
    include_invalid_mpich = False

    ompi_override_mpicc = os.environ.get("OMPI_ABI_TEST_MPICC_ABI")
    ompi_override_mpirun = os.environ.get("OMPI_ABI_TEST_MPIRUN")
    ompi_include_override = os.environ.get("OMPI_ABI_TEST_INCLUDE_PATH")
    ompi_library_override = os.environ.get("OMPI_ABI_TEST_LIBRARY_PATH")
    if include_open_mpi:
        if ompi_override_mpicc:
            include_invalid_open_mpi = True
            open_mpi_candidates.append(
                _open_mpi_candidate(
                    ompi_override_mpicc,
                    ompi_override_mpirun,
                    mpirun_override=bool(ompi_override_mpirun),
                    explicit_override=True,
                    include_override=ompi_include_override,
                    library_override=ompi_library_override))
        else:
            ompi_override_mpirun_path = _executable_path(ompi_override_mpirun)
            for mpicc_abi in _all_named_executables(
                    ("mpicc_abi",), "OMPI_ABI_TEST_MPIRUN"):
                candidate_mpirun = None
                if _same_tool_directory(mpicc_abi, ompi_override_mpirun_path):
                    candidate_mpirun = ompi_override_mpirun_path
                elif ompi_override_mpirun:
                    continue
                open_mpi_candidates.append(
                    _open_mpi_candidate(
                        mpicc_abi,
                        candidate_mpirun,
                        mpirun_override=bool(candidate_mpirun),
                        include_override=ompi_include_override,
                        library_override=ompi_library_override))
            include_invalid_open_mpi = not _has_valid_candidate(
                open_mpi_candidates)

    mpich_override_mpicc = os.environ.get("MPICH_ABI_TEST_MPICC")
    mpich_override_mpirun = os.environ.get("MPICH_ABI_TEST_MPIRUN")
    mpich_include_override = os.environ.get("MPICH_ABI_TEST_INCLUDE_PATH")
    mpich_library_override = os.environ.get("MPICH_ABI_TEST_LIBRARY_PATH")
    if include_mpich:
        if mpich_override_mpicc:
            include_invalid_mpich = True
            mpich_candidates.append(
                _mpich_candidate(
                    mpich_override_mpicc,
                    mpich_override_mpirun,
                    mpirun_override=bool(mpich_override_mpirun),
                    explicit_override=True,
                    include_override=mpich_include_override,
                    library_override=mpich_library_override))
        else:
            mpich_override_mpirun_path = _executable_path(mpich_override_mpirun)
            for mpicc in _all_named_executables(
                    ("mpicc_abi", "mpicc"), "MPICH_ABI_TEST_MPIRUN"):
                candidate_mpirun = None
                if _same_tool_directory(mpicc, mpich_override_mpirun_path):
                    candidate_mpirun = mpich_override_mpirun_path
                elif mpich_override_mpirun:
                    continue
                mpich_candidates.append(
                    _mpich_candidate(
                        mpicc,
                        candidate_mpirun,
                        mpirun_override=bool(candidate_mpirun),
                        include_override=mpich_include_override,
                        library_override=mpich_library_override))
            include_invalid_mpich = not _has_valid_candidate(mpich_candidates)

    return {
        "open_mpi": _open_mpi_report_candidates(
            open_mpi_candidates,
            include_invalid=include_invalid_open_mpi),
        "mpich": _mpich_report_candidates(
            mpich_candidates,
            include_invalid=include_invalid_mpich),
    }


def _select_install(discovery, implementation):
    """Return the selected installation of implementation.

    Discovery normally records only valid PATH candidates.  Explicit
    overrides also keep invalid candidates so reports can show exactly
    what the operator requested instead of silently falling back to PATH.
    """
    for candidate in discovery.get(implementation, []):
        if candidate.get("valid"):
            return candidate
    candidates = discovery.get(implementation, [])
    if candidates:
        return candidates[0]
    return None


def _open_mpi_mpifort(open_mpi_install):
    """Return the Open MPI Fortran wrapper matching the selected install.

    `check-abi-mpich` commonly puts MPICH before Open MPI on `PATH` so
    that the MPICH C wrapper can be discovered.  A plain `which mpifort`
    would then find MPICH's Fortran wrapper even though all installed
    Fortran ABI regression probes are intended to use the selected Open
    MPI installation.  Only an explicit override is allowed to break the
    same-prefix preference.
    """
    override = os.environ.get("OMPI_ABI_TEST_MPIFORT")
    if override:
        return _executable_path(override)
    if open_mpi_install is not None:
        bin_dir = open_mpi_install.get("bin_dir")
        path = _path_executable(bin_dir, "mpifort") if bin_dir else None
        if path is not None:
            return path
    return None
