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

"""Validate the generated LLM-friendly documentation artifacts.

Run after generate-llm-docs.py (e.g. via "make check" in docs/).  Checks:

* every JSONL catalog record validates against openmpi-mpi-api.schema.json;
* the manifest validates against openmpi-docs-manifest.schema.json;
* the cross-field invariant that each record's `languages` equals the distinct
  `bindings[].language` set (JSON Schema cannot express this);
* every record links back to the human docs (urls.html present);
* generated Markdown contains no unresolved RST (`.. include::` directives,
  `:ref:` roles, RST `\\`text <url>\\`_` hyperlinks, or man-page `|...|`
  substitutions);
* the manifest inventories every artifact except itself, with matching sha256
  and byte size, and lists nothing that is missing on disk;
* the committed sample records (specs/llms-friendly-docs/sample-records.jsonl)
  validate against the schema and match the generated catalog;
* every MPI Forum procedure that Open MPI actually implements has a man page,
  so an implemented MPI API is never silently undocumented.

Exits non-zero if any check fails.  Targets the Python 3.6 floor.
"""

import argparse
import hashlib
import json
import os
import re
import sys

try:
    import jsonschema
    # The schemas are JSON Schema draft 2020-12, which needs jsonschema >= 4
    # (Python 3.7+). On the Python 3.6 floor only jsonschema 3.2.0 installs,
    # and it lacks Draft202012Validator -- degrade gracefully there rather
    # than crashing on the missing attribute.
    HAVE_JSONSCHEMA = hasattr(jsonschema, 'Draft202012Validator')
except ImportError:
    HAVE_JSONSCHEMA = False

# Unresolved-RST patterns that must NOT appear in generated Markdown.
UNRESOLVED = [
    (":ref: role", re.compile(r':ref:`')),
    (".. include:: directive", re.compile(r'^\s*\.\.\s+include::', re.M)),
    ("RST directive", re.compile(r'^\s*\.\.\s+[a-zA-Z0-9_-]+::', re.M)),
    ("RST hyperlink", re.compile(r'`[^`]+<[^`>]+>`_')),
    ("man-page substitution",
     re.compile(r'\|(?:mdash|rarrow|deprecated_favor|ompi_ver|ompi_series|'
                r'mpi_standard_version|mpi_standard_major_version|'
                r'mpi_standard_minor_version)\|')),
]


def fail(errors, msg):
    errors.append(msg)


def read_text(path):
    with open(path, encoding='utf-8') as fp:
        return fp.read()


def read_bytes(path):
    with open(path, 'rb') as fp:
        return fp.read()


def read_json(path):
    return json.loads(read_text(path))


def main():
    ap = argparse.ArgumentParser(description="Validate LLM-friendly docs")
    ap.add_argument('--srcdir', required=True, help='docs source dir')
    ap.add_argument('--llms-dir', required=True,
                    help='published/staged llms/ directory to validate')
    ap.add_argument('--samples',
                    help='path to sample-records.jsonl (optional)')
    args = ap.parse_args()

    srcdir = os.path.abspath(args.srcdir)
    llms = os.path.abspath(args.llms_dir)
    schema_dir = os.path.join(srcdir, 'llms-src')
    errors = []

    cat_validator = man_validator = None
    if HAVE_JSONSCHEMA:
        catalog_schema = read_json(
            os.path.join(schema_dir, 'openmpi-mpi-api.schema.json'))
        manifest_schema = read_json(
            os.path.join(schema_dir, 'openmpi-docs-manifest.schema.json'))
        jsonschema.Draft202012Validator.check_schema(catalog_schema)
        jsonschema.Draft202012Validator.check_schema(manifest_schema)
        cat_validator = jsonschema.Draft202012Validator(catalog_schema)
        man_validator = jsonschema.Draft202012Validator(manifest_schema)
    else:
        print("  WARNING: jsonschema (>= 4, with Draft202012Validator) not "
              "available; skipping JSON Schema validation (cross-field, "
              "manifest, and Markdown checks still run). Install a recent "
              "jsonschema for full validation.", file=sys.stderr)

    # --- catalog records ---
    catalog_path = os.path.join(llms, 'openmpi-mpi-api.jsonl')
    catalog = {}
    n_records = 0
    for i, line in enumerate(read_text(catalog_path).split('\n'), 1):
        line = line.rstrip('\n')
        if not line:
            continue
        n_records += 1
        rec = json.loads(line)
        catalog[rec['name']] = line
        if cat_validator:
            for e in cat_validator.iter_errors(rec):
                fail(errors, "catalog record {} ({}): {}".format(
                    i, rec.get('name'), e.message))
        # cross-field invariant
        decl = sorted(rec['languages'])
        present = sorted({b['language'] for b in rec['bindings']})
        if decl != present:
            fail(errors, "catalog record {}: languages {} != bindings langs {}"
                 .format(rec['name'], decl, present))
        # link-back
        if not rec.get('urls', {}).get('html'):
            fail(errors, "catalog record {}: missing urls.html".format(
                rec['name']))
        # docset must not carry build identity (that lives only in the manifest)
        for bad in ('git_commit', 'git_describe', 'generated_at'):
            if bad in rec.get('docset', {}):
                fail(errors, "catalog record {}: docset contains build-identity "
                     "field {}".format(rec['name'], bad))

    # --- manifest ---
    manifest_path = os.path.join(llms, 'openmpi-docs-manifest.json')
    manifest = read_json(manifest_path)
    if man_validator:
        for e in man_validator.iter_errors(manifest):
            fail(errors, "manifest: {}".format(e.message))

    # --- manifest <-> on-disk consistency ---
    # The published root is the parent of llms/ (llms.txt lives there too).
    pub_root = os.path.dirname(llms)
    manifest_rel = 'llms/openmpi-docs-manifest.json'
    listed = set()
    for art in manifest['artifacts']:
        rel = art['path']
        listed.add(rel)
        if rel == manifest_rel:
            fail(errors, "manifest lists itself ({})".format(rel))
        full = os.path.join(pub_root, rel)
        if not os.path.exists(full):
            fail(errors, "manifest lists missing artifact: {}".format(rel))
            continue
        data = read_bytes(full)
        if hashlib.sha256(data).hexdigest() != art['sha256']:
            fail(errors, "manifest sha256 mismatch: {}".format(rel))
        if len(data) != art['bytes']:
            fail(errors, "manifest byte size mismatch: {}".format(rel))

    # Every on-disk LLM artifact (everything under llms/, plus the top-level
    # llms.txt) except the manifest must be listed.  Scope to the LLM tree so
    # this works whether llms/ sits alone in a staging dir or inside the full
    # Sphinx HTML output.
    on_disk = set()
    for root, _dirs, files in os.walk(llms):
        for fn in files:
            on_disk.add(os.path.relpath(os.path.join(root, fn), pub_root))
    if os.path.exists(os.path.join(pub_root, 'llms.txt')):
        on_disk.add('llms.txt')
    for rel in sorted(on_disk):
        if rel == manifest_rel:
            continue
        if rel not in listed:
            fail(errors, "artifact on disk not in manifest: {}".format(rel))

    # --- versioned llms.txt must be timestamp-free (so it is reproducible and
    #     listable in the manifest) ---
    llms_txt = os.path.join(pub_root, 'llms.txt')
    if os.path.exists(llms_txt):
        txt = read_text(llms_txt)
        if re.search(r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}', txt) \
                or 'generated_at' in txt:
            fail(errors, "llms.txt contains a generation timestamp "
                 "(it must be timestamp-free)")

    # --- no unresolved RST in generated Markdown ---
    for root, _dirs, files in os.walk(llms):
        for fn in files:
            if not fn.endswith('.md'):
                continue
            # Curated sources are hand-written Markdown, not RST-derived.
            if fn in ('openmpi-mpi-interface-guide.md',
                      'openmpi-mpi-examples.md',
                      'openmpi-runtime-introspection.md'):
                continue
            text = read_text(os.path.join(root, fn))
            for label, pat in UNRESOLVED:
                if pat.search(text):
                    fail(errors, "{}: unresolved {}".format(fn, label))

    # --- sample records ---
    samples = args.samples or os.path.join(
        srcdir, '..', 'specs', 'llms-friendly-docs', 'sample-records.jsonl')
    if os.path.exists(samples):
        for i, line in enumerate(read_text(samples).split('\n'), 1):
            line = line.rstrip('\n')
            if not line:
                continue
            rec = json.loads(line)
            if cat_validator:
                for e in cat_validator.iter_errors(rec):
                    fail(errors, "sample {} ({}): {}".format(
                        i, rec.get('name'), e.message))
            if rec['name'] not in catalog:
                fail(errors, "sample {} not in generated catalog".format(
                    rec['name']))
            elif catalog[rec['name']] != line:
                fail(errors, "sample {} differs from generated catalog record "
                     "(regenerate the samples)".format(rec['name']))

    # --- coverage: an implemented MPI API must have a man page ---
    # A procedure in the MPI Forum metadata that Open MPI actually implements
    # must have a man page; otherwise it is silently absent from the rendered
    # HTML docs, the installed Unix man pages, and these artifacts.  Whether a
    # procedure is "implemented" is read from the public C header
    # (ompi/include/mpi.h.in) and, for Fortran-only routines such as
    # MPI_F_sync_reg, from the use-mpi-f08 sources.  Procedures the MPI Standard
    # defines but Open MPI does not implement are intentionally not flagged.
    # Skipped (with a warning) when pympistandard or the ompi/ source tree is
    # unavailable, e.g. a docs-only checkout.
    top_srcdir = os.path.dirname(srcdir)
    mpih = os.path.join(top_srcdir, 'ompi', 'include', 'mpi.h.in')
    std = None
    try:
        import ompi_docs_common as common
        std = common.load_pympistandard(srcdir)
    except Exception as exc:
        print("  WARNING: pympistandard unavailable ({}); skipping "
              "implemented-API man-page coverage check".format(exc),
              file=sys.stderr)
    if std is not None and not os.path.exists(mpih):
        print("  WARNING: {} not found; skipping implemented-API man-page "
              "coverage check".format(mpih), file=sys.stderr)
    elif std is not None:
        directives = common.read_rst_man_pages(srcdir)
        documented = set(directives)
        for procs in directives.values():
            documented.update(procs)
        undocumented = [p for p in sorted(std.PROCEDURES)
                        if p not in documented]
        if undocumented:
            mpih_l = read_text(mpih).lower()
            f08_parts = []
            f08_dir = os.path.join(top_srcdir, 'ompi', 'mpi', 'fortran',
                                   'use-mpi-f08')
            for root, _dirs, files in os.walk(f08_dir):
                for fn in files:
                    if fn.endswith(('.F90', '.f90', '.h', '.in')):
                        f08_parts.append(
                            read_text(os.path.join(root, fn)).lower())
            f08_l = '\n'.join(f08_parts)
            for proc in undocumented:
                in_c = re.search(r'\b' + re.escape(proc) + r'\s*\(', mpih_l)
                in_f08 = re.search(r'\b' + re.escape(proc) + r'_f08\b', f08_l)
                if not (in_c or in_f08):
                    continue
                p = std.PROCEDURES[proc]
                binding = (common.c_binding(p) or common.f08_binding(p)
                           or common.f90_binding(p) or '')
                m = re.search(r'(MPI[A-Za-z0-9_]*)\s*\(', binding)
                pname = m.group(1) if m else proc
                fail(errors, "implemented MPI API has no man page: {0} "
                     "(add docs/man-openmpi/man3/{0}.3.rst)".format(pname))

    if errors:
        print("LLM docs validation FAILED ({} error(s)):".format(len(errors)),
              file=sys.stderr)
        for e in errors[:50]:
            print("  - {}".format(e), file=sys.stderr)
        if len(errors) > 50:
            print("  ... and {} more".format(len(errors) - 50), file=sys.stderr)
        sys.exit(1)

    print("  CHECK LLM docs: OK ({} records, manifest + samples validated)"
          .format(n_records))


if __name__ == "__main__":
    main()
