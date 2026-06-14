#!/usr/bin/env python3
#
# Copyright (c) 2026 Jeffrey M. Squyres.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Shared MPI documentation metadata helpers.

This module factors out logic that is common to the Open MPI
documentation generators:

* ``docs/generate-mpi-man3-bindings.py`` (the human man-page bindings), and
* ``docs/generate-llm-docs.py`` (the LLM-friendly artifacts).

It provides:

* parsing of the top-level ``VERSION`` file (without importing
  ``docs/conf.py``, which would trigger Sphinx configuration side effects);
* loading of the embedded ``pympistandard`` library plus
  ``docs/mpi-standard-apis.json``;
* parsing of the ``.. mpi-bindings:`` man-page directives that mark pages
  documenting multiple MPI procedures;
* rendering of the C (``iso_c``), fixed-form Fortran (``f90``), and
  ``use mpi_f08`` (``f08``) binding strings, including the large-count
  ("embiggened") variants; and
* build-identity helpers (git commit / ``git describe``) and a
  ``SOURCE_DATE_EPOCH``-aware UTC timestamp.

All code here must remain compatible with Open MPI's supported Python
version floor (see ``python_min_version`` in the top-level ``VERSION``
file): Python 3.6.
"""

import os
import re
import subprocess
import sys

from datetime import datetime, timezone
from pathlib import Path


# ---------------------------------------------------------------------------
# VERSION file parsing
# ---------------------------------------------------------------------------

def read_version_file(path):
    """Parse an Open MPI-style VERSION file into a dict.

    Uses the same rules as ``docs/conf.py`` (strip ``#`` comments, keep
    ``key=value`` lines), but does not import ``conf.py`` so that no Sphinx
    configuration side effects are triggered.
    """
    if not os.path.exists(path):
        raise FileNotFoundError("Unable to find VERSION file {}".format(path))

    data = {}
    with open(path, encoding='utf-8') as fp:
        for line in fp.readlines():
            if '#' in line:
                line = line.split("#")[0]
            line = line.strip()
            if '=' not in line:
                continue
            key, val = line.split("=", 1)
            data[key.strip()] = val.strip()
    return data


def ompi_version_info(top_srcdir):
    """Return a dict of assembled Open MPI version strings.

    ``top_srcdir`` is the top of the Open MPI source tree (the directory
    that contains the ``VERSION`` file). The returned dict includes:

    * ``ompi_version``: e.g. ``v6.1.0a1``
    * ``ompi_series``:  e.g. ``v6.1.x``
    * ``mpi_standard_version``: e.g. ``4.1``
    """
    data = read_version_file(os.path.join(top_srcdir, "VERSION"))
    major = data["major"]
    minor = data["minor"]
    release = data["release"]
    greek = data.get("greek", "")
    info = {
        "major": major,
        "minor": minor,
        "release": release,
        "greek": greek,
        "ompi_version": "v{}.{}.{}{}".format(major, minor, release, greek),
        "ompi_series": "v{}.{}.x".format(major, minor),
    }
    if "mpi_standard_version" in data and "mpi_standard_subversion" in data:
        info["mpi_standard_version"] = "{}.{}".format(
            data["mpi_standard_version"], data["mpi_standard_subversion"])
        info["mpi_standard_major_version"] = data["mpi_standard_version"]
        info["mpi_standard_minor_version"] = data["mpi_standard_subversion"]
    info["python_min_version"] = data.get("python_min_version")
    return info


# ---------------------------------------------------------------------------
# pympistandard loading
# ---------------------------------------------------------------------------

def load_pympistandard(docs_srcdir):
    """Import and initialize the embedded ``pympistandard`` library.

    ``docs_srcdir`` is the ``docs/`` source directory. The ``pympistandard``
    package lives in the Open MPI ``3rd-party`` tree, and the MPI Standard
    API JSON ships as ``docs/mpi-standard-apis.json`` (see
    ``generate-mpi-man3-bindings.py`` for provenance).

    Returns the imported ``pympistandard`` module, already initialized with
    ``use_api_version(1, ...)``.
    """
    docs_srcdir = os.path.abspath(docs_srcdir)
    top_srcdir = os.path.dirname(docs_srcdir)

    pympistandard_dir = Path(os.path.join(
        top_srcdir, "3rd-party", "pympistandard", "src")).resolve()
    sys.path.insert(0, str(pympistandard_dir))

    import pympistandard as std

    mpi_standard_json = os.path.join(docs_srcdir, "mpi-standard-apis.json")
    std.use_api_version(1, given_path=mpi_standard_json)
    return std


# ---------------------------------------------------------------------------
# .. mpi-bindings: directive parsing
# ---------------------------------------------------------------------------

def read_rst_man_pages(src_dir):
    """Scan ``src_dir/man-openmpi/man3`` for ``.. mpi-bindings:`` directives.

    Some ``MPI_*.3.rst`` man pages document several MPI procedures via a
    ``.. mpi-bindings: MPI_Foo, MPI_Bar`` comment line (a single-colon
    comment, not a registered Sphinx directive). Returns a dict mapping each
    man-page API name (lowercased, without ``.3.rst``) to the list of
    lowercased procedure names documented on that page.
    """
    directives = {}
    prog = re.compile(r'^MPI_.*\.3\.rst$')

    man3_dir = Path(os.path.join(src_dir, 'man-openmpi', 'man3')).resolve()
    for file in os.listdir(man3_dir):
        if not prog.match(file):
            continue

        with open(os.path.join(man3_dir, file), encoding='utf-8') as fp:
            lines = fp.readlines()

        file_api_name = file.replace('.3.rst', '').lower()
        directives[file_api_name] = list()

        prefix = '.. mpi-bindings:'
        for line in lines:
            line = line.strip()
            if not line.startswith(prefix):
                continue

            bindings = line[len(prefix):].split(',')
            for binding in bindings:
                binding = binding.strip()
                directives[file_api_name].append(binding.lower())

    return directives


# ---------------------------------------------------------------------------
# Binding string rendering
# ---------------------------------------------------------------------------

def _valid_binding(binding):
    """An express binding is usable iff it is a non-empty, non-'None' string."""
    return binding is not None and len(binding) > 0 and binding != 'None'


def c_binding(data):
    """Return the C (``iso_c``) binding string for a procedure, or ``None``.

    Applies the workaround for
    https://github.com/mpi-forum/pympistandard/issues/25, where
    ``MPI_Pcontrol`` renders ``\\ldots`` instead of ``...``.
    """
    binding = str(data.express.iso_c)
    if not _valid_binding(binding):
        return None
    return binding.replace(r'\ldots', '...')


def c_binding_large(data):
    """Return the large-count C binding string, or ``None`` if none exists."""
    if not data.has_embiggenment():
        return None
    binding = str(data.express.embiggen.iso_c)
    if not _valid_binding(binding):
        return None
    return binding.replace(r'\ldots', '...')


def f90_binding(data):
    """Return the fixed-form Fortran (``f90``) binding string, or ``None``.

    The same string is used for both the ``mpif.h`` and ``use mpi``
    interfaces; they differ only in how the interface is accessed
    (``INCLUDE 'mpif.h'`` versus ``USE mpi``), not in the signature.
    """
    binding = str(data.express.f90)
    if not _valid_binding(binding):
        return None
    return binding


def f08_binding(data):
    """Return the ``use mpi_f08`` (``f08``) binding string, or ``None``."""
    binding = str(data.express.f08)
    if not _valid_binding(binding):
        return None
    return binding


def f08_binding_large(data):
    """Return the large-count ``f08`` binding string, or ``None``."""
    if not data.has_embiggenment():
        return None
    binding = str(data.express.embiggen.f08)
    if not _valid_binding(binding):
        return None
    return binding


# ---------------------------------------------------------------------------
# Build identity / reproducible timestamp
# ---------------------------------------------------------------------------

def _git(top_srcdir, args):
    """Run a git command in ``top_srcdir``; return stripped stdout or None."""
    try:
        out = subprocess.check_output(
            ['git'] + args, cwd=top_srcdir, stderr=subprocess.DEVNULL)
    except (OSError, subprocess.CalledProcessError):
        return None
    out = out.decode('utf-8', 'replace').strip()
    return out if out else None


def git_commit(top_srcdir):
    """Return the full source commit hash, or ``None`` if unavailable."""
    return _git(top_srcdir, ['rev-parse', 'HEAD'])


def git_describe(top_srcdir):
    """Return ``git describe --tags --always`` output, or ``None``.

    Mirrors how ``config/opal_get_version.sh`` resolves the repo revision.
    """
    return _git(top_srcdir, ['describe', '--tags', '--always'])


def source_date_epoch():
    """Return the reproducible build epoch (int seconds) honored project-wide.

    Honors ``SOURCE_DATE_EPOCH`` (see https://reproducible-builds.org/ and
    ``config/getdate.sh``); returns ``None`` if it is unset or unparsable so
    the caller can fall back gracefully (e.g. to a git commit date).
    """
    raw = os.environ.get('SOURCE_DATE_EPOCH')
    if raw is None:
        return None
    try:
        return int(raw)
    except ValueError:
        return None


def generated_at_utc(top_srcdir=None):
    """Return an ISO-8601 UTC timestamp string for artifact generation.

    Resolution order, mirroring the rest of the build:

    1. ``SOURCE_DATE_EPOCH`` if set (reproducible builds);
    2. the build commit's committer date, if a git checkout is available;
    3. the current wall-clock time.
    """
    epoch = source_date_epoch()
    if epoch is None and top_srcdir is not None:
        commit_epoch = _git(top_srcdir, ['log', '-1', '--format=%ct'])
        if commit_epoch is not None:
            try:
                epoch = int(commit_epoch)
            except ValueError:
                epoch = None
    if epoch is not None:
        dt = datetime.fromtimestamp(epoch, tz=timezone.utc)
    else:
        dt = datetime.now(tz=timezone.utc)
    return dt.strftime('%Y-%m-%dT%H:%M:%SZ')
