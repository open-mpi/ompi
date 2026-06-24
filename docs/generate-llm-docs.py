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

"""Generate LLM-friendly documentation artifacts for the Open MPI MPI APIs.

This produces, under a build-tree staging directory, the artifact tree that a
copy step in docs/Makefile.am publishes into the Sphinx HTML output after the
Sphinx run (not via ``html_extra_path``, since the staged Markdown must be
excluded from Sphinx source discovery and ``exclude_patterns`` would also
suppress an ``html_extra_path`` entry; see specs/llms-friendly-docs/spec.md):

  <staging>/
    llms.txt
    llms/
      openmpi-mpi-api.jsonl
      openmpi-mpi-api.md
      openmpi-mpi-api-c.md
      openmpi-mpi-api-fortran-mpifh.md
      openmpi-mpi-api-fortran-use-mpi.md
      openmpi-mpi-api-fortran-use-mpi-f08.md
      openmpi-mpi-interface-guide.md      (curated; copied from llms-src)
      openmpi-mpi-examples.md             (curated; copied from llms-src)
      openmpi-mpi-api.schema.json         (curated; copied from llms-src)
      openmpi-docs-manifest.schema.json   (curated; copied from llms-src)
      openmpi-docs-manifest.json
      man-openmpi/man3/MPI_<name>.3.md
      man-openmpi/man1/<command>.1.md      (command corpus; no JSONL records)

Build identity (git commit/describe, generation timestamp) lives only in the
manifest; every other artifact is wall-clock-free and deterministic at a given
commit.  Shared metadata logic comes from ompi_docs_common.py.

Targets the Python 3.6 floor (see python_min_version in VERSION).
"""

import argparse
import hashlib
import json
import os
import posixpath
import re

import ompi_docs_common as common

SCHEMA_VERSION = 1
ARTIFACT_SCHEMA_VERSION = 1


class LinkMaker:
    """Builds the URLs that the generated artifacts use to link to each other
    and to the human-facing HTML documentation.

    There are two consumers, and they want different link styles:

    * A **Read the Docs** build publishes to ``docs.open-mpi.org/en/<slug>/``.
      Links must be absolute and use that version slug, so a reader of the
      published site (or a record copied out of it) can resolve them.  Pass the
      absolute base (e.g. ``https://docs.open-mpi.org/en/main``) and every link
      becomes ``<base>/<target>``.

    * A **local build from git** produces the artifacts for a *local* LLM/tool
      that reads the files straight off disk.  There is no docs.open-mpi.org
      site in play, so the RTD URL scheme is irrelevant; what matters is that
      the tree is self-contained and portable.  Pass ``None`` and every link
      becomes relative to the file that contains it, so the local tree resolves
      no matter where it lives.

    Always feed :meth:`link` *logical* paths relative to the documentation
    version root (e.g. ``"llms/openmpi-mpi-api.jsonl"``,
    ``"man-openmpi/man3/MPI_Init.3.html"``) -- never real on-disk paths.  This
    keeps output independent of the output directory, which the determinism
    check relies on (it generates into two different directories and diffs).
    """

    def __init__(self, url_base):
        # Absolute base (no trailing slash) for an RTD/explicit build, or None
        # for a local build (relative links).
        self._base = url_base.rstrip('/') if url_base else None

    @property
    def absolute(self):
        return self._base is not None

    def link(self, target, source):
        """URL to ``target`` from the file ``source`` (both root-relative)."""
        if self._base is not None:
            return "{}/{}".format(self._base, target)
        return posixpath.relpath(target, posixpath.dirname(source) or '.')

LANG_C = "c"
LANG_MPIFH = "fortran_mpifh"
LANG_USE_MPI = "fortran_use_mpi"
LANG_F08 = "fortran_mpi_f08"

# Curated source files (committed under docs/llms-src/) copied verbatim into
# the published llms/ tree.
CURATED_FILES = [
    "openmpi-mpi-interface-guide.md",
    "openmpi-mpi-examples.md",
    "openmpi-runtime-introspection.md",
    "openmpi-mpi-api.schema.json",
    "openmpi-docs-manifest.schema.json",
]

MEDIA_TYPES = {
    ".md": "text/markdown",
    ".jsonl": "application/jsonl",
    ".json": "application/json",
    ".txt": "text/plain",
}

# Heuristic API-family categorization by name prefix.  Matching is by list
# order (the first matching prefix wins), so list more-specific prefixes
# before less-specific ones.
CATEGORY_PREFIXES = [
    ("MPI_T_", "tools"),
    ("MPI_File_", "io"),
    ("MPI_Win_", "rma"),
    ("MPI_Comm_", "communicator"),
    ("MPI_Group_", "group"),
    ("MPI_Type_", "datatype"),
    ("MPI_Session_", "sessions"),
    ("MPI_Info_", "info"),
    ("MPI_Cart_", "topology"),
    ("MPI_Graph_", "topology"),
    ("MPI_Dist_graph_", "topology"),
    ("MPI_Errhandler_", "error-handling"),
    ("MPI_Add_error", "error-handling"),
    ("MPI_Error", "error-handling"),
]
_COLLECTIVE_BASE = set("""
    MPI_Barrier MPI_Bcast MPI_Gather MPI_Gatherv MPI_Scatter MPI_Scatterv
    MPI_Allgather MPI_Allgatherv MPI_Alltoall MPI_Alltoallv MPI_Alltoallw
    MPI_Reduce MPI_Allreduce MPI_Reduce_scatter MPI_Reduce_scatter_block
    MPI_Scan MPI_Exscan
    MPI_Neighbor_allgather MPI_Neighbor_allgatherv MPI_Neighbor_alltoall
    MPI_Neighbor_alltoallv MPI_Neighbor_alltoallw
""".split())


def _expand_collectives(bases):
    """Expand blocking collective names to their nonblocking (MPI_I...) and
    persistent (..._init) variants. Over-generation is harmless: names that do
    not correspond to a real procedure simply never match."""
    out = set()
    for n in bases:
        out.add(n)
        out.add("MPI_I" + n[4].lower() + n[5:])
        out.add(n + "_init")
    return out


COLLECTIVE_NAMES = _expand_collectives(_COLLECTIVE_BASE)
POINT_TO_POINT_NAMES = set("""
    MPI_Send MPI_Recv MPI_Sendrecv MPI_Sendrecv_replace MPI_Bsend MPI_Ssend
    MPI_Rsend MPI_Isend MPI_Irecv MPI_Ibsend MPI_Issend MPI_Irsend
    MPI_Probe MPI_Iprobe MPI_Mprobe MPI_Improbe MPI_Mrecv MPI_Imrecv
    MPI_Wait MPI_Waitall MPI_Waitany MPI_Waitsome MPI_Test MPI_Testall
    MPI_Testany MPI_Testsome MPI_Get_count MPI_Cancel MPI_Start MPI_Startall
    MPI_Send_init MPI_Bsend_init MPI_Ssend_init MPI_Rsend_init MPI_Recv_init
    MPI_Isendrecv MPI_Isendrecv_replace
""".split())
# Process-management / environment procedures.
PROCESS_NAMES = set("""
    MPI_Init MPI_Init_thread MPI_Finalize MPI_Initialized MPI_Finalized
    MPI_Query_thread MPI_Is_thread_main MPI_Abort MPI_Get_processor_name
    MPI_Get_version MPI_Get_library_version MPI_Wtime MPI_Wtick MPI_Pcontrol
    MPI_Get_hw_resource_info
""".split())
# One-sided (RMA) communication procedures whose names do not start with
# MPI_Win_ (window-management names are matched by the MPI_Win_ prefix).
RMA_NAMES = set("""
    MPI_Get MPI_Put MPI_Accumulate MPI_Get_accumulate MPI_Fetch_and_op
    MPI_Compare_and_swap MPI_Rget MPI_Rput MPI_Raccumulate MPI_Rget_accumulate
""".split())


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def setup_cli():
    parser = argparse.ArgumentParser(
        description="Generate LLM-friendly docs for the Open MPI MPI APIs")
    parser.add_argument('--srcdir', required=True, help='docs source dir')
    parser.add_argument('--builddir', required=True, help='docs build dir')
    parser.add_argument('--outdir', default=None,
                        help='staging output dir (default: <builddir>/llms-build)')
    parser.add_argument('--url-base', default=None,
                        help='absolute public docs URL base (e.g. '
                             'https://docs.open-mpi.org/en/main); forces '
                             'absolute links. Default: absolute from the Read '
                             'the Docs environment if present, otherwise '
                             'relative links for a local build')
    return parser.parse_args()


# ---------------------------------------------------------------------------
# RST -> Markdown conversion (closed vocabulary; see spec)
# ---------------------------------------------------------------------------

HEADING_LEVELS = {'=': 1, '-': 2, '^': 3, '~': 4, '"': 5, '+': 6, '#': 1, '*': 2}
UNDERLINE_RE = re.compile(r'^([=\-^~"+#*])\1{2,}\s*$')
DIRECTIVE_RE = re.compile(r'^(\s*)\.\.\s+([a-zA-Z0-9_-]+)::\s*(.*)$')
COMMENT_RE = re.compile(r'^(\s*)\.\.\s+(.*)$')
INCLUDE_RE = re.compile(r'^\s*\.\.\s+include::\s*(\S+)\s*$')
OPTION_RE = re.compile(r'^\s+:[a-zA-Z-]+:.*$')

# Substitutions used in the man pages (whitelist; values filled from VERSION).
STATIC_SUBS = {
    'mdash': '—',
    'rarrow': '→',
    'deprecated_favor': 'this routine is deprecated in favor of',
}


def build_substitutions(version_info):
    subs = dict(STATIC_SUBS)
    if 'mpi_standard_version' in version_info:
        subs['mpi_standard_version'] = version_info['mpi_standard_version']
        subs['mpi_standard_major_version'] = version_info['mpi_standard_major_version']
        subs['mpi_standard_minor_version'] = version_info['mpi_standard_minor_version']
    subs['ompi_ver'] = version_info['ompi_version']
    subs['ompi_series'] = version_info['ompi_series']
    return subs


def resolve_include_path(target, including_file, srcdir, builddir):
    """Resolve an ``.. include::`` target to an existing file path or None."""
    base = os.path.dirname(including_file)
    cand = os.path.normpath(os.path.join(base, target))
    candidates = [cand]
    # Generated bindings live in the build tree; map srcdir->builddir.
    srcdir = os.path.abspath(srcdir)
    builddir = os.path.abspath(builddir)
    ac = os.path.abspath(cand)
    if ac.startswith(srcdir):
        candidates.append(os.path.join(builddir, os.path.relpath(ac, srcdir)))
    if ac.startswith(builddir):
        candidates.append(os.path.join(srcdir, os.path.relpath(ac, builddir)))
    for c in candidates:
        if os.path.isfile(c):
            return c
    return None


def expand_includes(text, including_file, srcdir, builddir, _seen=None):
    """Recursively inline ``.. include::`` directives, honoring :start-after:."""
    if _seen is None:
        _seen = set()
    out = []
    lines = text.split('\n')
    i = 0
    while i < len(lines):
        line = lines[i]
        m = INCLUDE_RE.match(line)
        if not m:
            out.append(line)
            i += 1
            continue

        target = m.group(1)
        # Collect option lines (e.g., :start-after:) that follow the include.
        start_after = None
        i += 1
        while i < len(lines) and OPTION_RE.match(lines[i]):
            opt = lines[i].strip()
            mo = re.match(r':start-after:\s*(.*)$', opt)
            if mo:
                start_after = mo.group(1).strip()
            i += 1

        path = resolve_include_path(target, including_file, srcdir, builddir)
        if path is None or path in _seen:
            # Unresolvable or cyclic include: drop it (do not leak the directive).
            continue
        with open(path, encoding='utf-8') as fp:
            inc = fp.read()
        if start_after is not None:
            idx = inc.find(start_after)
            if idx >= 0:
                inc = inc[idx + len(start_after):]
        nested = expand_includes(inc, path, srcdir, builddir,
                                 _seen | {path})
        out.append(nested)
    return '\n'.join(out)


def make_ref_resolver(known_pages, linkmaker, source):
    """Return a function that turns :ref: roles into Markdown links/text.

    ``source`` is the root-relative path of the file whose body is being
    rendered (e.g. ``"llms/man-openmpi/man3/x"``), so that in a local
    (relative-link) build the man-page links resolve correctly from it.
    """
    # O(1) case-insensitive lookup: labels are often lowercase (e.g. mpi_send).
    lower_map = {p.lower(): p for p in known_pages}

    def html_url(name):
        return linkmaker.link("man-openmpi/man3/{}.3.html".format(name), source)

    ref_re = re.compile(r':ref:`([^`<]+?)(?:\s*<([^`>]+)>)?`')

    def repl(m):
        text = m.group(1).strip()
        target = (m.group(2) or text).strip()
        # Normalize a target like "MPI_Send(3)" or label "mpi_send".
        cand = target.replace('(3)', '').strip()
        page = cand if cand in known_pages else lower_map.get(cand.lower())
        if page is not None:
            return "[{}]({})".format(text, html_url(page))
        return text

    def resolve(text):
        return ref_re.sub(repl, text)

    return resolve


HYPERLINK_RE = re.compile(r'`([^`<]+?)\s*<([^`>]+)>`_+')
# :doc:`Title </path>` or :doc:`/path` -- the target doc is human-facing RST,
# not part of the LLM corpus, so reduce the role to its display text.
DOC_RE = re.compile(r':doc:`([^`<]+?)(?:\s*<[^`>]+>)?`')


def resolve_inline(text, subs, ref_resolve):
    # :ref: roles first (may span wrapped lines).
    text = ref_resolve(text)
    # :doc: roles -> display text (the target is not in the LLM corpus).
    text = DOC_RE.sub(lambda m: m.group(1).strip(), text)
    # RST external hyperlink: `text <url>`_ (or `__`) -> [text](url).
    text = HYPERLINK_RE.sub(
        lambda m: '[{}]({})'.format(m.group(1).strip(), m.group(2).strip()), text)
    # Substitutions (whitelist only -> literal text like MPI_{Wait|Test} is safe).
    def sub_repl(m):
        name = m.group(1)
        return subs.get(name, m.group(0))
    text = re.sub(r'\|([a-zA-Z_]+)\|', sub_repl, text)
    # RST inline literal ``x`` -> Markdown `x` (collapse double backticks).
    text = text.replace('``', '`')
    return text


def _dedent(block_lines):
    indents = [len(l) - len(l.lstrip(' ')) for l in block_lines if l.strip()]
    n = min(indents) if indents else 0
    return [l[n:] if len(l) >= n else l for l in block_lines]


def rst_to_markdown(text, subs, ref_resolve):
    """Convert the (already include-expanded) RST text to Markdown.

    Prose lines are buffered and resolved as a paragraph so that inline
    constructs that wrap across source lines (multi-line :ref: roles, RST
    hyperlinks) are handled correctly. Code-block bodies are emitted verbatim
    and never passed through inline resolution.
    """
    lines = text.split('\n')
    out = []
    para = []

    def flush_para():
        if para:
            joined = '\n'.join(para)
            out.append(resolve_inline(joined, subs, ref_resolve))
            del para[:]

    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]

        # Blank line: paragraph boundary.
        if not line.strip():
            flush_para()
            out.append('')
            i += 1
            continue

        # Section heading: text line followed by an underline of punctuation.
        if (i + 1 < n and UNDERLINE_RE.match(lines[i + 1])
                and not line.lstrip().startswith(('.. ', '* ', '- '))):
            flush_para()
            level = HEADING_LEVELS.get(lines[i + 1].strip()[0], 2)
            title = resolve_inline(line.strip(), subs, ref_resolve)
            out.append('#' * level + ' ' + title)
            out.append('')
            i += 2
            continue

        # Directives with a "::" (code-block, seealso, note, etc.).
        dm = DIRECTIVE_RE.match(line)
        if dm:
            flush_para()
            indent, name, arg = dm.group(1), dm.group(2), dm.group(3).strip()
            i += 1
            # Skip any directive option lines, then a single blank line.
            while i < n and OPTION_RE.match(lines[i]):
                i += 1
            if i < n and not lines[i].strip():
                i += 1
            # Gather the indented body of the directive.
            body = []
            base = len(indent)
            while i < n:
                cur = lines[i]
                if not cur.strip():
                    body.append('')
                    i += 1
                    continue
                cur_indent = len(cur) - len(cur.lstrip(' '))
                if cur_indent <= base:
                    break
                body.append(cur)
                i += 1
            while body and not body[-1].strip():
                body.pop()
            body = _dedent(body)

            if name in ('code-block', 'code'):
                lang = arg if arg else ''
                out.append('```' + lang)
                out.extend(body)
                out.append('```')
                out.append('')
            elif name == 'math':
                out.append('```math')
                out.extend(body)
                out.append('```')
                out.append('')
            elif name in ('note', 'admonition', 'warning', 'important'):
                if name == 'admonition':
                    label = arg if arg else 'Note'
                    inline = None
                else:
                    label = name.capitalize()
                    inline = arg if arg else None  # content on the directive line
                out.append('> **{}:**'.format(label))
                # Resolve the whole body as one block (joined with newlines) so
                # inline constructs that wrap across source lines -- multi-line
                # :ref: roles and `text <url>`_ hyperlinks -- are handled, just
                # like the top-level paragraph buffer above.
                block = ([inline] if inline else []) + body
                resolved = resolve_inline('\n'.join(block), subs, ref_resolve)
                for b in resolved.split('\n'):
                    out.append('> ' + b if b.strip() else '>')
                out.append('')
            elif name == 'seealso':
                out.append('## See also')
                out.append('')
                for b in body:
                    if b.strip():
                        out.append(resolve_inline(b, subs, ref_resolve))
                out.append('')
            elif name in ('toctree',):
                pass  # navigation: drop entirely
            else:
                # Unknown directive (e.g., list-table): keep its body as text.
                for b in body:
                    if b.strip():
                        out.append(resolve_inline(b, subs, ref_resolve))
                if body:
                    out.append('')
            continue

        # Bare comment / anchor (".. _label:", ".. include_body",
        # ".. mpi-bindings:", ".. some prose"): drop the comment and its
        # indented continuation.
        cm = COMMENT_RE.match(line)
        if cm and not line.strip().startswith('.. |'):
            flush_para()
            base = len(cm.group(1))
            i += 1
            while i < n and (not lines[i].strip()
                             or (len(lines[i]) - len(lines[i].lstrip(' ')) > base)):
                if not lines[i].strip():
                    break
                i += 1
            continue

        # RST literal block: a line ending in '::' introduces an indented
        # verbatim block.  Emit it as a fenced code block (verbatim, no inline
        # resolution) rather than leaking the '::' marker into the Markdown.
        stripped = line.rstrip()
        if stripped.endswith('::'):
            k = i + 1
            while k < n and not lines[k].strip():
                k += 1
            cur_indent = len(line) - len(line.lstrip(' '))
            if k < n and (len(lines[k]) - len(lines[k].lstrip(' '))) > cur_indent:
                lead = stripped[:-2]
                lead = lead.rstrip() if (not lead or lead.endswith(' ')) \
                    else lead + ':'
                if lead:
                    para.append(lead)
                flush_para()
                j = k
                body = []
                while j < n:
                    cur = lines[j]
                    if not cur.strip():
                        body.append('')
                        j += 1
                        continue
                    if (len(cur) - len(cur.lstrip(' '))) <= cur_indent:
                        break
                    body.append(cur)
                    j += 1
                while body and not body[-1].strip():
                    body.pop()
                out.append('```')
                out.extend(_dedent(body))
                out.append('```')
                out.append('')
                i = j
                continue

        # Ordinary prose line: buffer until a paragraph boundary.
        para.append(line)
        i += 1

    flush_para()

    # Collapse 3+ blank lines to a single blank line.
    md = '\n'.join(out)
    md = re.sub(r'\n{3,}', '\n\n', md)
    return md.strip() + '\n'


# ---------------------------------------------------------------------------
# Per-symbol Markdown page
# ---------------------------------------------------------------------------

def page_markdown(page_name, expanded, subs, ref_resolve, linkmaker, source_rel,
                  page_rel):
    """Render a per-symbol man3 page.  ``page_rel`` is the root-relative path of
    the file this Markdown will live in, so its Canonical-HTML link resolves
    correctly in a local (relative-link) build."""
    body = rst_to_markdown(expanded, subs, ref_resolve)
    html = linkmaker.link("man-openmpi/man3/{}.3.html".format(page_name),
                          page_rel)
    header = (
        "<!-- Generated from {src}. Canonical HTML: {html} -->\n\n"
        .format(src=source_rel, html=html))
    return header + body


# ---------------------------------------------------------------------------
# Parameter / binding extraction
# ---------------------------------------------------------------------------

def parse_param_descriptions(rst_text):
    """Return {param_name: description} from INPUT/OUTPUT PARAMETER bullets."""
    descs = {}
    bullet = re.compile(r'^\*\s+``([^`]+)``:\s*(.*)$')
    for line in rst_text.split('\n'):
        m = bullet.match(line.strip())
        if m:
            descs[m.group(1).strip()] = m.group(2).strip()
    return descs


def _normalize_direction(d):
    if d is None:
        return 'unknown'
    val = d.value if hasattr(d, 'value') else str(d)
    val = val.split('.')[-1].lower()
    return val if val in ('in', 'out', 'inout') else 'unknown'


# Maps a binding language to the suppress-token that hides a parameter from
# that language's signature, so each binding can be given only its own
# parameters (e.g. ierror appears in the Fortran bindings but not in C;
# argc/argv appear only in C).
_LANG_SUPPRESS_TOKEN = {
    LANG_C: 'c_parameter',
    LANG_MPIFH: 'f90_parameter',
    LANG_USE_MPI: 'f90_parameter',
    LANG_F08: 'f08_parameter',
}


def _fortran_intent(param_direction):
    """Map a semantic direction to a Fortran INTENT attribute string."""
    d = (param_direction or '').lower()
    if d in ('in', 'out', 'inout'):
        return 'INTENT({})'.format(d.upper())
    return None


def normalized_parameters(proc, param_descs, subs, ref_resolve):
    """Build the normalized parameter list for a procedure.

    Iterates the *full* per-procedure parameter metadata rather than the LIS
    view: the LIS view (``express.lis``) suppresses parameters that do not
    appear in the language-independent signature (e.g. MPI_Init's argc/argv
    and ierror are all LIS-suppressed), which would drop real parameters and
    yield empty/partial lists.  Using the full set keeps the catalog
    signature/parameter authoritative.

    Each returned entry carries the public schema fields plus, under a
    ``_meta`` key, the upstream per-parameter facts (``suppress`` set and the
    F08 ``optional`` flag, plus C const-ness) that :func:`standard_bindings`
    needs to render an accurate *per-binding* parameter list.  ``_meta`` is
    stripped before serialization (see :func:`_public_param`).

    At the record level, ``type`` is the MPI semantic kind name (language
    neutral) and ``direction`` is the semantic direction.  ``intent`` and
    ``optional`` are left non-committal at the record level (``null`` and the
    schema-sanctioned ``"unknown"`` sentinel respectively) because both are
    per-binding properties; the precise values are emitted per binding.

    Human descriptions come from the RST parameter bullets when present,
    falling back to the upstream metadata description (and its kind default).
    """
    # The full parameter set (with semantic kinds) is only available on the
    # raw parseset; the public binding views each filter a different subset.
    # KINDS / the kind default-description map come from the same embedded
    # pympistandard package that ``proc`` belongs to.
    from pympistandard.storage import KINDS
    from pympistandard.lis import _DEFAULT_DESCRIPTIONS

    params = []
    for p in proc._parseset.get('parameters', []):
        name = p['name']
        direction = _normalize_direction(p.get('lis_direction'))
        kind_key = p.get('kind')
        kind = KINDS[kind_key] if kind_key is not None else None
        kind_name = getattr(kind, 'name', None) if kind is not None else None
        desc = param_descs.get(name)
        if not desc:
            desc = p.get('desc') or None
        if not desc and kind_name is not None:
            desc = _DEFAULT_DESCRIPTIONS[kind_name] or None
        if desc:
            desc = resolve_inline(desc, subs, ref_resolve)
        params.append({
            "name": name,
            "direction": direction,
            "type": kind_name,
            "intent": None,
            "optional": "unknown",
            "description": desc,
            "_meta": {
                "suppress": set((p.get('suppress') or '').split()),
                "optional": bool(p.get('optional')),
                "constant": bool(p.get('constant')),
                "param_direction": p.get('param_direction'),
            },
        })
    return params


def _public_param(param):
    """Return a copy of a normalized parameter without the internal ``_meta``."""
    return {k: v for k, v in param.items() if k != '_meta'}


def _binding_parameters(params, language):
    """Project the normalized parameter list onto a single binding.

    Drops parameters suppressed for ``language`` and fills in the per-binding
    ``intent`` (Fortran INTENT / C const-ness) and ``optional`` (the F08
    OPTIONAL attribute) that are ambiguous on the language-neutral list.
    """
    token = _LANG_SUPPRESS_TOKEN.get(language)
    out = []
    for p in params:
        meta = p['_meta']
        if token is not None and token in meta['suppress']:
            continue
        bp = _public_param(p)
        if language == LANG_C:
            bp['intent'] = 'const' if meta['constant'] else None
            # C has no optional-argument concept for these parameters.
            bp['optional'] = False
        elif language == LANG_F08:
            bp['intent'] = _fortran_intent(meta['param_direction'])
            bp['optional'] = meta['optional']
        else:  # mpif.h / use mpi: positional, no OPTIONAL attribute
            bp['intent'] = _fortran_intent(meta['param_direction'])
            bp['optional'] = False
        out.append(bp)
    return out


def standard_bindings(proc, params, name):
    """Build the bindings array for a standard procedure (proper-case name).

    Each binding gets only its own parameters (projected from the
    language-neutral ``params`` via :func:`_binding_parameters`): e.g. the C
    binding excludes the Fortran-only ``ierror`` and the Fortran bindings
    exclude the C-only ``argc``/``argv``.
    """
    bindings = []

    c = common.c_binding(proc)
    if c is not None:
        c_params = _binding_parameters(params, LANG_C)
        bindings.append({"language": LANG_C, "procedure": name,
                         "signature": c, "large_count": False,
                         "return_convention": "int return code",
                         "parameters": c_params})
        clarge = common.c_binding_large(proc)
        if clarge is not None:
            bindings.append({"language": LANG_C, "procedure": name + "_c",
                             "signature": clarge, "large_count": True,
                             "return_convention": "int return code",
                             "parameters": c_params})

    f90 = common.f90_binding(proc)
    if f90 is not None:
        for lang in (LANG_MPIFH, LANG_USE_MPI):
            bindings.append({"language": lang, "procedure": name,
                             "signature": f90, "large_count": False,
                             "return_convention": "ierror argument",
                             "parameters": _binding_parameters(params, lang)})

    f08 = common.f08_binding(proc)
    if f08 is not None:
        f08_params = _binding_parameters(params, LANG_F08)
        bindings.append({"language": LANG_F08, "procedure": name,
                         "signature": f08, "large_count": False,
                         "return_convention": "ierror argument",
                         "parameters": f08_params})
        f08large = common.f08_binding_large(proc)
        if f08large is not None:
            bindings.append({"language": LANG_F08, "procedure": name,
                             "signature": f08large, "large_count": True,
                             "return_convention": "ierror argument",
                             "parameters": f08_params})
    return bindings


def categorize(name):
    if name in COLLECTIVE_NAMES:
        return "collective"
    if name in POINT_TO_POINT_NAMES:
        return "point-to-point"
    if name in PROCESS_NAMES:
        return "process"
    if name in RMA_NAMES:
        return "rma"
    for prefix, cat in CATEGORY_PREFIXES:
        if name.startswith(prefix):
            return cat
    if name.startswith(("MPIX_", "OMPI_")):
        return "extension"
    return "other"


def _extract_declaration(code_lines, name):
    """Return only the C declaration of ``name`` from a code block's lines.

    Extension/deprecated man pages sometimes put several function declarations
    in one SYNTAX code block (e.g. MPIX_Comm_agree and MPIX_Comm_iagree); a
    per-procedure record should carry only its own signature, not all of them.
    Falls back to the whole block if ``name`` is not found.
    """
    pat = re.compile(r'\b' + re.escape(name) + r'\s*\(')
    start = None
    for i, ln in enumerate(code_lines):
        if pat.search(ln):
            start = i
            break
    if start is None:
        return '\n'.join(code_lines)
    out = []
    depth = 0
    for ln in code_lines[start:]:
        out.append(ln)
        depth += ln.count('(') - ln.count(')')
        if depth <= 0:
            break
    return '\n'.join(out)


def extension_bindings_from_rst(rst_text, name=None):
    """Best-effort: pull the C signature out of an extension page's code block.

    If ``name`` is given and the block declares several procedures, return only
    the declaration matching ``name``.
    """
    lines = rst_text.split('\n')
    for i, line in enumerate(lines):
        m = DIRECTIVE_RE.match(line)
        if m and m.group(2) in ('code-block', 'code'):
            base = len(m.group(1))
            j = i + 1
            while j < len(lines) and not lines[j].strip():
                j += 1
            # Body lines are those indented deeper than the directive itself
            # (man-page code blocks use a 3-space indent, not necessarily 4).
            body = []
            while j < len(lines):
                cur = lines[j]
                if not cur.strip():
                    j += 1
                    continue
                indent = len(cur) - len(cur.lstrip(' \t'))
                if indent <= base:
                    break
                body.append(cur.strip())
                j += 1
            code_lines = [b for b in body if not b.startswith('#')]
            if name:
                sig = _extract_declaration(code_lines, name).strip()
            else:
                sig = '\n'.join(code_lines).strip()
            if sig:
                return [{"language": LANG_C, "procedure": None,
                         "signature": sig, "large_count": False,
                         "return_convention": "int return code",
                         "parameters": []}]
    return []


def parse_seealso(rst_text):
    """Collect the symbol names referenced in a page's .. seealso:: block."""
    names = []
    lines = rst_text.split('\n')
    in_block = False
    for line in lines:
        if line.strip().startswith('.. seealso::'):
            in_block = True
            continue
        if in_block:
            if line.strip() and not line.startswith((' ', '\t')):
                break
            for m in re.finditer(r':ref:`([^`<]+?)(?:\s*<[^`>]+>)?`', line):
                nm = m.group(1).strip().replace('(3)', '').strip()
                if nm and nm not in names:
                    names.append(nm)
    return names


def short_description(rst_text, subs, ref_resolve):
    """Extract the one-line short description that follows the page title."""
    lines = rst_text.split('\n')
    i = 0
    n = len(lines)
    # Skip to past the title underline.
    while i + 1 < n and not (lines[i].strip() and UNDERLINE_RE.match(lines[i + 1])):
        i += 1
    i += 2
    # Find the first non-blank, non-directive paragraph.
    buf = []
    while i < n:
        s = lines[i].strip()
        if not s:
            if buf:
                break
            i += 1
            continue
        if s.startswith('.. ') or s == '::':
            # Skip directives and a bare RST literal-block marker; the real
            # short description may follow inside the literal block.
            i += 1
            continue
        buf.append(s)
        i += 1
    text = ' '.join(buf)
    text = resolve_inline(text, subs, ref_resolve)
    # Drop a leading "NAME — " prefix for brevity.
    text = re.sub(r'^\[?[A-Za-z_0-9]+\]?(\([^)]*\))?\s*[—-]\s*', '', text)
    return text.strip()


def best_effort_record(name, kind, category, expanded, docset, urls,
                       source_rel, errors_html, documented_with, bindings=None):
    """Build a best-effort catalog record for a procedure whose metadata is not
    in pympistandard (Open MPI extensions, or deprecated/removed MPI APIs).

    The signature is recovered from the man-page RST rather than the structured
    metadata, so structured parameter fields are left empty (``unknown``).
    """
    if bindings is None:
        bindings = extension_bindings_from_rst(expanded, name)
    for b in bindings:
        b["procedure"] = name
    languages = sorted({b['language'] for b in bindings})
    return {
        "schema_version": SCHEMA_VERSION,
        "project": "open-mpi",
        "docset": docset,
        "name": name,
        "kind": kind,
        "standard": None,
        "category": category,
        "languages": languages,
        "bindings": bindings,
        "parameters": [],
        "semantics": None,
        "errors": {"convention": None, "url": errors_html},
        "seealso": parse_seealso(expanded),
        "documented_with": documented_with,
        "urls": urls,
        "sources": {"rst": source_rel, "binding_metadata": "rst"},
    }


# ---------------------------------------------------------------------------
# Manifest helpers
# ---------------------------------------------------------------------------

def media_type_for(path):
    return MEDIA_TYPES.get(os.path.splitext(path)[1], 'application/octet-stream')


def estimate_tokens(data_bytes):
    # Coarse heuristic: ~4 bytes per token.
    return (len(data_bytes) + 3) // 4


# Absolute paths of every artifact written this run; used to build the
# manifest and to prune stale files left from previous runs.
WRITTEN_PATHS = []


def write_file(path, text):
    path = os.path.abspath(path)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    data = text.encode('utf-8')
    WRITTEN_PATHS.append(path)
    # Only write when changed (deterministic, avoids needless churn / mtime
    # bumps so incremental docs builds don't see spurious changes).
    if os.path.exists(path):
        with open(path, 'rb') as fp:
            if fp.read() == data:
                return
    with open(path, 'wb') as fp:
        fp.write(data)


def prune_stale(outdir):
    """Remove files under outdir that were not written this run, plus empties."""
    keep = set(WRITTEN_PATHS)
    for root, _dirs, files in os.walk(outdir):
        for fn in files:
            full = os.path.abspath(os.path.join(root, fn))
            if full not in keep:
                os.remove(full)
    for root, _dirs, _files in os.walk(outdir, topdown=False):
        if os.path.abspath(root) != os.path.abspath(outdir) and not os.listdir(root):
            os.rmdir(root)


# ---------------------------------------------------------------------------
# Command (man1) man pages
# ---------------------------------------------------------------------------

def command_man_pages(srcdir, builddir, llms_dir, subs, ref_resolve, linkmaker):
    """Convert the section-1 command man pages (man1) to Markdown.

    These document Open MPI commands (mpirun, ompi_info, the wrapper
    compilers, ...), not MPI APIs, so they get a Markdown corpus only -- no
    JSONL catalog records.  Returns the sorted list of page names written
    (e.g. "mpirun.1") for indexing in llms.txt.  The bare ".so" redirect
    stubs (mpicc.1 -> ompi-wrapper-compiler.1) are not RST and are skipped.

    ``ref_resolve`` must resolve :ref: links from the man1 page location, and
    the Canonical-HTML header link is built from that same location, so a local
    (relative-link) build resolves correctly.
    """
    man1_src = os.path.join(srcdir, 'man-openmpi', 'man1')
    if not os.path.isdir(man1_src):
        return []
    pages = []
    for fn in sorted(os.listdir(man1_src)):
        if fn == 'index.rst' or not fn.endswith('.1.rst'):
            continue
        name = fn[:-len('.rst')]  # "mpirun.1.rst" -> "mpirun.1"
        rst_path = os.path.join(man1_src, fn)
        source_rel = os.path.relpath(rst_path, srcdir)
        page_rel = "llms/man-openmpi/man1/{}.md".format(name)
        with open(rst_path, encoding='utf-8') as fp:
            raw = fp.read()
        expanded = expand_includes(raw, rst_path, srcdir, builddir)
        body = rst_to_markdown(expanded, subs, ref_resolve)
        html = linkmaker.link("man-openmpi/man1/{}.html".format(name), page_rel)
        header = ("<!-- Generated from {src}. Canonical HTML: {html} -->\n\n"
                  .format(src=source_rel, html=html))
        write_file(os.path.join(llms_dir, 'man-openmpi', 'man1',
                                '{}.md'.format(name)), header + body)
        pages.append(name)
    return pages


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    args = setup_cli()
    srcdir = os.path.abspath(args.srcdir)
    builddir = os.path.abspath(args.builddir)
    top_srcdir = os.path.dirname(srcdir)
    outdir = os.path.abspath(args.outdir) if args.outdir \
        else os.path.join(builddir, 'llms-build')
    llms_dir = os.path.join(outdir, 'llms')

    version_info = common.ompi_version_info(top_srcdir)

    # Resolve the link strategy for this build (see the LinkMaker docstring).
    #
    # A local build from git produces the artifacts for a *local* LLM/tool that
    # reads the files straight off disk: there is no docs.open-mpi.org site in
    # play, so the RTD URL scheme is irrelevant and links are made RELATIVE to
    # each file (a self-contained, portable tree).  A Read the Docs build
    # publishes to docs.open-mpi.org/en/<slug>/, so there links are ABSOLUTE and
    # use that published version slug (e.g. .../en/main/ for the "main" branch).
    # An explicit --url-base / OMPI_LLM_URL_BASE override forces absolute links.
    rtd_slug = os.environ.get('READTHEDOCS_VERSION')
    explicit_base = args.url_base or os.environ.get('OMPI_LLM_URL_BASE')
    if explicit_base:
        url_base = explicit_base
    elif os.environ.get('READTHEDOCS_CANONICAL_URL'):
        url_base = os.environ['READTHEDOCS_CANONICAL_URL']
    elif rtd_slug:
        url_base = "https://docs.open-mpi.org/en/{}".format(rtd_slug)
    else:
        url_base = None  # local build -> relative links
    lm = LinkMaker(url_base)

    subs = build_substitutions(version_info)
    std = common.load_pympistandard(srcdir)

    man3_src = os.path.join(srcdir, 'man-openmpi', 'man3')
    page_re = re.compile(r'^((?:MPI_|MPIX_|OMPI_).*)\.3\.rst$')
    page_files = {}
    for fn in sorted(os.listdir(man3_src)):
        m = page_re.match(fn)
        if m:
            page_files[m.group(1)] = os.path.join(man3_src, fn)
    known_pages = set(page_files.keys())
    # :ref: links must resolve from the location of the file they end up in.
    # The per-symbol man3 pages, the man1 pages, and the files that live
    # directly under llms/ (aggregate corpus, language corpora, JSONL records)
    # are at different depths, so each gets a resolver bound to its own
    # location.  (In an absolute/RTD build all three produce identical links.)
    res_man3 = make_ref_resolver(known_pages, lm, "llms/man-openmpi/man3/x")
    res_man1 = make_ref_resolver(known_pages, lm, "llms/man-openmpi/man1/x")
    res_llms = make_ref_resolver(known_pages, lm, "llms/x")

    # Map each procedure (lowercase) to its co-documented procedures.
    directives = common.read_rst_man_pages(srcdir)
    lower_to_actual = {p.lower(): p for p in known_pages}
    co_docs = {}
    for _page, procs in directives.items():
        if not procs:
            continue
        for p in procs:
            co_docs[p] = [lower_to_actual.get(q, q) for q in procs if q != p]

    docset = {"ompi_version": version_info['ompi_version'],
              "ompi_series": version_info['ompi_series'],
              "schema_version": SCHEMA_VERSION}
    # The JSONL catalog lives at llms/openmpi-mpi-api.jsonl, so its record URLs
    # are resolved from there.
    jsonl_rel = "llms/openmpi-mpi-api.jsonl"
    errors_html = lm.link("man-openmpi/man3/MPI_Errors.3.html", jsonl_rel)

    records = []
    page_md = {}      # page_name -> per-symbol-page markdown (man3 depth)
    agg_md = {}       # page_name -> same body rendered for the llms/ corpus
    short_desc = {}   # page_name -> short description

    for name in sorted(page_files):
        rst_path = page_files[name]
        source_rel = os.path.relpath(rst_path, srcdir)
        with open(rst_path, encoding='utf-8') as fp:
            raw = fp.read()
        expanded = expand_includes(raw, rst_path, srcdir, builddir)

        page_rel = "llms/man-openmpi/man3/{}.3.md".format(name)
        page_md[name] = page_markdown(name, expanded, subs, res_man3,
                                      lm, source_rel, page_rel)
        # The same body is inlined into the aggregate corpus at llms/, a
        # different depth, so in a relative-link build it must be re-rendered
        # from that location; in an absolute build the links are identical, so
        # reuse the per-symbol render.
        if lm.absolute:
            agg_md[name] = page_md[name]
        else:
            agg_md[name] = page_markdown(name, expanded, subs, res_llms,
                                         lm, source_rel,
                                         "llms/openmpi-mpi-api.md")
        short_desc[name] = short_description(expanded, subs, res_llms)

        low = name.lower()
        urls = {
            "html": lm.link("man-openmpi/man3/{}.3.html".format(name),
                            jsonl_rel),
            "markdown": lm.link("llms/man-openmpi/man3/{}.3.md".format(name),
                                jsonl_rel),
        }
        if low in std.PROCEDURES:
            proc = std.PROCEDURES[low]
            params = normalized_parameters(
                proc, parse_param_descriptions(expanded), subs, res_llms)
            bindings = standard_bindings(proc, params, name)
            params = [_public_param(p) for p in params]
            languages = sorted({b['language'] for b in bindings})
            rec = {
                "schema_version": SCHEMA_VERSION,
                "project": "open-mpi",
                "docset": docset,
                "name": name,
                "kind": "standard",
                "standard": {"version": None},
                "category": categorize(name),
                "languages": languages,
                "bindings": bindings,
                "parameters": params,
                "semantics": {
                    "blocking": None,
                    "collective": name in COLLECTIVE_NAMES,
                    "local": None,
                    "initialization_required": None,
                    "threading_notes": None,
                },
                "errors": {
                    "convention": "C: int return code; Fortran: ierror argument.",
                    "url": errors_html,
                },
                "seealso": parse_seealso(expanded),
                "documented_with": co_docs.get(low, []),
                "urls": urls,
                "sources": {"rst": source_rel, "binding_metadata": "pympistandard"},
            }
            records.append(rec)
        elif name.startswith(("MPIX_", "OMPI_")):
            records.append(best_effort_record(
                name, "extension", "extension", expanded, docset, urls,
                source_rel, errors_html, co_docs.get(low, [])))
        else:
            # An MPI_* page not known to pympistandard documents a
            # deprecated/removed procedure when a signature can be recovered
            # from its RST; otherwise it is an overview/non-procedure page
            # (e.g. MPI_T.3, MPI_Errors.3) and gets Markdown only, no record.
            ext_bindings = (extension_bindings_from_rst(expanded, name)
                            if name.startswith("MPI_") else [])
            # Only a real procedure page: its recovered signature must actually
            # declare this name (filters overview pages such as MPI_T.3 whose
            # code blocks contain unrelated example code).
            declares = ext_bindings and re.search(
                r'\b' + re.escape(name) + r'\s*\(', ext_bindings[0]['signature'])
            if declares:
                records.append(best_effort_record(
                    name, "deprecated", categorize(name), expanded, docset,
                    urls, source_rel, errors_html, co_docs.get(low, []),
                    bindings=ext_bindings))

    records.sort(key=lambda r: r['name'])

    # --- Write per-symbol Markdown pages ---
    for name in sorted(page_md):
        write_file(os.path.join(llms_dir, 'man-openmpi', 'man3',
                                '{}.3.md'.format(name)), page_md[name])

    # --- Write command (man1) Markdown pages (corpus only, no records) ---
    man1_pages = command_man_pages(srcdir, builddir, llms_dir, subs,
                                   res_man1, lm)

    # --- Write the JSONL catalog ---
    jsonl = ''.join(json.dumps(r, ensure_ascii=False, sort_keys=True) + '\n'
                    for r in records)
    write_file(os.path.join(llms_dir, 'openmpi-mpi-api.jsonl'), jsonl)

    # --- Aggregate Markdown corpora ---
    header = ("# Open MPI MPI API reference ({ver})\n\n"
              "Generated for LLM and retrieval consumers. The MPI Standard is "
              "authoritative for portable MPI semantics; `MPIX_*` and `OMPI_*` "
              "are Open MPI extensions.\n\n"
              .format(ver=version_info['ompi_version']))
    all_md = header + '\n\n---\n\n'.join(agg_md[n] for n in sorted(agg_md))
    write_file(os.path.join(llms_dir, 'openmpi-mpi-api.md'), all_md)

    lang_corpora = {
        LANG_C: 'openmpi-mpi-api-c.md',
        LANG_MPIFH: 'openmpi-mpi-api-fortran-mpifh.md',
        LANG_USE_MPI: 'openmpi-mpi-api-fortran-use-mpi.md',
        LANG_F08: 'openmpi-mpi-api-fortran-use-mpi-f08.md',
    }
    lang_titles = {
        LANG_C: 'C', LANG_MPIFH: "Fortran (mpif.h)",
        LANG_USE_MPI: "Fortran (use mpi)", LANG_F08: "Fortran (use mpi_f08)",
    }
    lang_fence = {LANG_C: 'c', LANG_MPIFH: 'fortran',
                  LANG_USE_MPI: 'fortran', LANG_F08: 'fortran'}
    for lang, fname in lang_corpora.items():
        parts = ["# Open MPI MPI API reference — {} interface ({})\n"
                 .format(lang_titles[lang], version_info['ompi_version'])]
        for rec in records:
            blist = [b for b in rec['bindings'] if b['language'] == lang]
            if not blist:
                continue
            parts.append("## {}\n".format(rec['name']))
            sd = short_desc.get(rec['name'])
            if sd:
                parts.append(sd + "\n")
            for b in blist:
                parts.append("```{}\n{}\n```\n".format(lang_fence[lang],
                                                       b['signature']))
            parts.append("Documentation: {}\n".format(rec['urls']['html']))
        write_file(os.path.join(llms_dir, fname), '\n'.join(parts) + '\n')

    # --- Copy curated source files (those that exist) ---
    llms_src = os.path.join(srcdir, 'llms-src')
    present_curated = set()
    for fn in CURATED_FILES:
        src = os.path.join(llms_src, fn)
        if os.path.isfile(src):
            with open(src, 'rb') as fp:
                write_file(os.path.join(llms_dir, fn),
                           fp.read().decode('utf-8'))
            present_curated.add(fn)

    # --- versioned llms.txt (timestamp-free, listable in the manifest) ---
    llms_txt = build_llms_txt(version_info, lm, rtd_slug, present_curated,
                              man1_pages)
    write_file(os.path.join(outdir, 'llms.txt'), llms_txt)

    # --- Manifest (the only artifact carrying build identity) ---
    manifest = build_manifest(outdir, lm, version_info, top_srcdir)
    write_file(os.path.join(llms_dir, 'openmpi-docs-manifest.json'),
               json.dumps(manifest, ensure_ascii=False, indent=1,
                          sort_keys=True) + '\n')

    # Remove any stale files from a previous run so the published tree matches
    # the manifest exactly.
    prune_stale(outdir)

    print("  GENERATE LLM docs ({} records, {} pages)"
          .format(len(records), len(page_md)))


def _this_build_description(version_info, linkmaker, rtd_slug):
    """One sentence identifying which build this llms.txt belongs to.

    For a Read the Docs "main" build the slug is "main" but the version it
    represents is the VERSION-file series, so both are named (the dual
    attribution requested in the spec).  A local build has no published slug;
    it is identified by its version and noted as relative/local.
    """
    series = version_info['ompi_series']        # e.g. "v6.1.x"
    version = version_info['ompi_version']       # e.g. "6.1.0"
    if not linkmaker.absolute:
        return ("a local build of Open MPI {} (the {} series), built from a git "
                "checkout. The links in this file are relative to this file's "
                "own location: this build targets a local tool that reads these "
                "files directly off disk, so it deliberately does not use the "
                "docs.open-mpi.org URL scheme.".format(version, series))
    if rtd_slug == 'main':
        return ('the "main" git branch (the development tip), which currently '
                'identifies itself as the Open MPI {} series. It is published '
                'at https://docs.open-mpi.org/en/main/ .'.format(series))
    if rtd_slug:
        return ('Open MPI {slug}, published at '
                'https://docs.open-mpi.org/en/{slug}/ .'.format(slug=rtd_slug))
    return 'Open MPI {} (the {} series).'.format(version, series)


def build_llms_txt(version_info, linkmaker, rtd_slug, present_curated,
                   man1_pages=None):
    v = version_info['ompi_version']

    def link(target):
        # All links in llms.txt are resolved from the file's own location
        # (the documentation version root, where llms.txt lives).
        return linkmaker.link(target, "llms.txt")

    lines = [
        "# Open MPI {}".format(v), "",
        "Open MPI is an open source implementation of the Message Passing "
        "Interface (MPI) specification. This file indexes LLM-friendly "
        "documentation artifacts for this documentation version.", "",
        "## About this file", "",
        "This is the llms.txt for {}".format(
            _this_build_description(version_info, linkmaker, rtd_slug)), "",
        "Open MPI publishes a separate set of these LLM-friendly artifacts for "
        "each documentation version. For Open MPI v5.0.0 and later they all "
        "follow the same URL scheme:", "",
        "    https://docs.open-mpi.org/en/VERSION_SLUG/", "",
        "where VERSION_SLUG is either:", "",
        "- `main` --- the build from Open MPI's `main` git branch (the "
        "development tip); or",
        "- `vA.B.C` --- where A and B are the major and minor version numbers. "
        "If C is `x` (e.g. `v6.1.x`) it is the build from that release branch; "
        "if C is an integer (e.g. `v6.1.0`) it is the build from that specific "
        "git-tagged release.", "",
        "If you found this file but are looking for the LLM documentation of a "
        "different Open MPI version, substitute the desired VERSION_SLUG into "
        "the scheme above (for example, "
        "https://docs.open-mpi.org/en/main/llms.txt).", "",
        "Documentation for Open MPI versions older than v5.0.0 is not published "
        "in this format; for those, see "
        "https://github.com/open-mpi/ompi/blob/v4.1.x/README , "
        "https://www.open-mpi.org/faq/ , and https://www.open-mpi.org/doc/ .",
        "",
        "- The MPI Standard (https://www.mpi-forum.org/docs/) is authoritative "
        "for portable MPI semantics.",
        "- `MPIX_*` and `OMPI_*` APIs are Open MPI extensions, not portable "
        "MPI Standard APIs.", "",
        "## API catalog and corpora", "",
        "- [Machine-readable API catalog (JSONL)]({})".format(
            link("llms/openmpi-mpi-api.jsonl")),
        "- [Aggregate Markdown corpus]({})".format(
            link("llms/openmpi-mpi-api.md")),
        "- [C interface]({})".format(link("llms/openmpi-mpi-api-c.md")),
        "- [Fortran mpif.h interface]({})".format(
            link("llms/openmpi-mpi-api-fortran-mpifh.md")),
        "- [Fortran use mpi interface]({})".format(
            link("llms/openmpi-mpi-api-fortran-use-mpi.md")),
        "- [Fortran use mpi_f08 interface]({})".format(
            link("llms/openmpi-mpi-api-fortran-use-mpi-f08.md")),
    ]
    # Only advertise curated artifacts that are actually published.
    if 'openmpi-mpi-interface-guide.md' in present_curated:
        lines.append("- [Interface selection guide]({})".format(
            link("llms/openmpi-mpi-interface-guide.md")))
    if 'openmpi-mpi-examples.md' in present_curated:
        lines.append("- [Curated examples]({})".format(
            link("llms/openmpi-mpi-examples.md")))
    lines += [
        "- [Artifact manifest]({})".format(
            link("llms/openmpi-docs-manifest.json")), "",
    ]
    # Runtime introspection: the artifacts above describe the API for this
    # version; a *specific installation* (version, build config, available
    # components, and the run-time MCA parameters its components expose) is
    # discovered from that installation with the ompi_info command.
    if 'openmpi-runtime-introspection.md' in present_curated:
        lines += [
            "## Runtime introspection (installed Open MPI)", "",
            "The artifacts above describe the MPI API for this version. To "
            "inspect a *specific* installation --- its exact version, build "
            "configuration, available MCA components, and the full set of "
            "run-time tunables (MCA parameters) --- run the `ompi_info` command "
            "on that installation; `ompi_info --all --parsable` emits a "
            "comprehensive, self-describing, machine-readable dump.", "",
            "- [Runtime introspection with ompi_info]({})".format(
                link("llms/openmpi-runtime-introspection.md")), "",
        ]
    # Command (man1) man pages: a Markdown corpus, no catalog records.
    if man1_pages:
        lines += ["## Command man pages", ""]
        lines += ["- [{n}]({u})".format(
            n=n, u=link("llms/man-openmpi/man1/{}.md".format(n)))
            for n in man1_pages]
        lines += [""]
    lines += [
        "## Human-facing documentation", "",
        "- [Building MPI applications]({})".format(
            link("building-apps/index.html")),
        "- [Launching MPI applications]({})".format(
            link("launching-apps/index.html")),
        "- [Tuning MPI applications]({})".format(
            link("tuning-apps/index.html")),
    ]
    return '\n'.join(lines) + '\n'


ALL_LANGS_LIST = [LANG_C, LANG_MPIFH, LANG_USE_MPI, LANG_F08]
# Artifacts whose content is specific to a language/interface set.
ARTIFACT_LANGUAGES = {
    'llms/openmpi-mpi-api.md': ALL_LANGS_LIST,
    'llms/openmpi-mpi-api.jsonl': ALL_LANGS_LIST,
    'llms/openmpi-mpi-api-c.md': [LANG_C],
    'llms/openmpi-mpi-api-fortran-mpifh.md': [LANG_MPIFH],
    'llms/openmpi-mpi-api-fortran-use-mpi.md': [LANG_USE_MPI],
    'llms/openmpi-mpi-api-fortran-use-mpi-f08.md': [LANG_F08],
}


def build_manifest(outdir, linkmaker, version_info, top_srcdir):
    manifest_rel = 'llms/openmpi-docs-manifest.json'
    # Inventory exactly the artifacts written this run (so stale leftovers are
    # never listed). The manifest is not yet written here, so it is naturally
    # excluded; guard against it anyway.
    artifacts = []
    seen = set()
    for full in WRITTEN_PATHS:
        rel = os.path.relpath(full, outdir)
        if rel == manifest_rel or rel in seen:
            continue
        seen.add(rel)
        with open(full, 'rb') as fp:
            data = fp.read()
        entry = {
            # path stays root-relative (the validator locates files by it);
            # only the URL follows the build's link strategy.
            "path": rel,
            "url": linkmaker.link(rel, manifest_rel),
            "media_type": media_type_for(full),
            "sha256": hashlib.sha256(data).hexdigest(),
            "bytes": len(data),
            "tokens_estimate": estimate_tokens(data),
        }
        mm = re.match(r'llms/man-openmpi/man3/(.+)\.3\.md$', rel)
        if mm:
            entry["symbols"] = [mm.group(1)]
        if rel in ARTIFACT_LANGUAGES:
            entry["languages"] = ARTIFACT_LANGUAGES[rel]
        artifacts.append(entry)
    artifacts.sort(key=lambda e: e['path'])

    manifest = {
        "artifact_schema_version": ARTIFACT_SCHEMA_VERSION,
        "project": "open-mpi",
        "ompi_version": version_info['ompi_version'],
        "ompi_series": version_info['ompi_series'],
        "rtd_version_slug": os.environ.get('READTHEDOCS_VERSION'),
        "git_commit": common.git_commit(top_srcdir),
        "git_describe": common.git_describe(top_srcdir),
        "generated_at": common.generated_at_utc(top_srcdir),
        "artifacts": artifacts,
    }
    return manifest


if __name__ == "__main__":
    main()
