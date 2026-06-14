# LLM-friendly documentation artifacts

Open MPI publishes human-oriented documentation as Sphinx-rendered HTML and Unix manual pages. This page specifies additional generated artifacts intended for consumption by Large Language Models (LLMs), retrieval systems, and coding assistants.

The initial scope is the public Open MPI MPI library APIs: C, `mpif.h`, `use mpi`, and `use mpi_f08`. The section-1 command man pages (`mpirun`, `ompi_info`, the wrapper compilers, ...) are also published as a Markdown corpus — these are commands rather than MPI APIs, so they get Markdown pages but no JSONL catalog records. OpenSHMEM is explicitly **deferred**: the same publication structure can later be applied to OpenSHMEM, but only once someone actually asks for it. This spec does not cover OpenSHMEM.

## Goals

The LLM-friendly documentation artifacts should:

- use the existing RST manual pages and MPI binding metadata as the source of truth;
- publish stable, versioned URLs on <https://docs.open-mpi.org/>;
- provide concise Markdown for text-oriented consumers;
- provide machine-readable API metadata for tools that generate or analyze MPI application code;
- distinguish MPI Standard APIs from Open MPI extensions such as `MPIX_*` and `OMPI_*`;
- be reproducible: identical sources at a given commit produce byte-identical semantic artifacts;
- avoid creating a second hand-maintained API reference.

## Non-goals

These artifacts are not a replacement for the MPI Standard, the human-facing Open MPI documentation, or the installed Unix manual pages. They should not document Open MPI internal implementation interfaces as if they were public application APIs. They do not add a new build-time dependency for users building from official release tarballs.

## Sources of truth and the data model

There are two distinct API populations, with two different extraction paths. Implementers must keep them separate.

### Standard MPI APIs (metadata-driven)

Standard MPI procedures are described by structured metadata that already drives `docs/generate-mpi-man3-bindings.py`:

- `3rd-party/pympistandard` (the MPI Forum Python library), loaded from the embedded source tree;
- `docs/mpi-standard-apis.json` (the MPI Forum `apis.json`, copied into the tree).

`pympistandard` exposes exactly **three** rendered binding strings per procedure: C (`iso_c`), fixed-form Fortran (`f90`), and `use mpi_f08` (`f08`). It also exposes per-parameter metadata (`name`, `param_direction`, `kind`, `optional`, `large_only`, `constant`, `desc`, …) and an "embiggened" (large-count) variant when one exists.

For standard APIs:

- **Structured parameter fields** (direction, kind/type, optionality, large-count) come from `mpi-standard-apis.json` / `pympistandard`. These are authoritative because they are machine-generated from the Standard.
- **Human parameter descriptions** come from the RST man page `INPUT PARAMETERS` / `OUTPUT PARAMETERS` / `INPUT/OUTPUT PARAMETERS` bullets when present, falling back to the JSON `desc` field otherwise.

### Extension APIs (RST-driven, best-effort)

Open MPI extensions (`MPIX_*`, e.g. the ULFM `MPIX_Comm_*` and `MPIX_Query_*_support` families; and `OMPI_*`, e.g. `OMPI_Affinity_str`) have hand-written man pages and are **not** present in `pympistandard`. For these:

- signature text is preserved **verbatim** from the RST `SYNTAX` code blocks;
- structured per-parameter fields (`direction`, `kind`, `intent`) are set to `unknown` (or omitted) when they cannot be reliably extracted;
- records still carry `name`, `kind: extension`, `languages`, human description, see-also, and URLs.

The catalog therefore mixes high-fidelity standard records with best-effort extension records; the `kind` field lets consumers tell them apart.

### Fortran interface model

The catalog uses four language tags — `c`, `fortran_mpifh`, `fortran_use_mpi`, `fortran_mpi_f08` — and produces four language-specific Markdown corpora. However, `mpif.h` and `use mpi` **share the same `f90` signature**; they differ only in the access preamble (`INCLUDE 'mpif.h'` vs `USE mpi`). The spec keeps them as separate tags/corpora for audience clarity and future divergence, but implementers should expect the `fortran_mpifh` and `fortran_use_mpi` signatures to be identical apart from that preamble, both derived from the single `f90` binding.

### Man page ↔ record mapping

- A per-symbol Markdown page is generated for **every** MPI/MPIX/OMPI man page, 1:1 with the human man pages, so canonical URLs line up.
- A JSONL catalog record is generated **per documented procedure**, not per page.
- Some man pages document **multiple** procedures via the `.. mpi-bindings:` comment directive (for example, `MPI_Alltoallw.3.rst` carries `MPI_Alltoallw`, `MPI_Ialltoallw`, and `MPI_Alltoallw_init`). Each such procedure also has its own thin man page that `.. include::`s the primary page body, so each procedure has its own canonical HTML URL even though the prose is shared. Each procedure gets its own catalog record and its own `urls`, plus a `documented_with` field naming the co-documented procedures.
- Some man pages are overviews or non-procedure pages (for example `MPI_T.3`, `Open-MPI.7`, `MPI_Errors.3`). These get a per-symbol Markdown page but **no** JSONL record.

The Markdown generator must handle **arbitrary** RST section headings, not a fixed schema: real man pages contain sections such as `NOTE` vs `NOTES`, `USE OF IN-PLACE OPTION`, and `WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR` in addition to the common ones.

## Publication model

Read the Docs supports serving a custom top-level `llms.txt` file from a project's default version, provided that the file exists in the HTML build output. Sphinx supports copying non-RST files into the HTML output with `html_extra_path`. Therefore Open MPI should publish:

- a top-level `https://docs.open-mpi.org/llms.txt` from the default Read the Docs version (note: this follows whatever Read the Docs' *default* version is, which may be a series that does not yet carry these artifacts);
- a versioned `llms.txt` in each public documentation version, such as `https://docs.open-mpi.org/en/<version>/llms.txt` (where `<version>` is the Read the Docs version slug, e.g. `v6.1.x` or `main`);
- generated LLM artifacts below each versioned documentation tree, such as `https://docs.open-mpi.org/en/<version>/llms/...`.

Each public documentation version emits a single, self-describing `llms.txt`; there is no separate hand-maintained top-level index file. The version-neutral `https://docs.open-mpi.org/llms.txt` is simply whatever the Read the Docs *default* version produced (which, per the note above, may be a series that does not yet carry these artifacts). Each versioned `llms.txt` therefore points to the artifacts of its own version and documents the `https://docs.open-mpi.org/en/VERSION_SLUG/` scheme so a consumer can reach any other version, rather than relying on a top-level index (the `llms.txt` section below details this).

If Open MPI later needs different top-level behavior, Read the Docs exact redirects can be used to redirect `/llms.txt` to a chosen versioned path.

The generated artifacts ride **inside** the per-version HTML output tree. The generator writes to a build-tree staging directory (`$(builddir)/llms-build/`), and a Sphinx `build-finished` hook in `conf.py` copies that tree into the HTML output (`app.outdir`) after the build. The copy is done in a Sphinx hook — rather than `html_extra_path` or a Makefile step — for two reasons: (1) the staged Markdown must be kept out of Sphinx source discovery via `exclude_patterns`, which would also suppress an `html_extra_path` entry pointing at it; and (2) the hook runs inside `sphinx-build`, so it works identically for `make` builds **and** for Read the Docs, which runs `sphinx-build` directly and never runs `make`. This is deliberate: riding the HTML output tree means the artifacts inherit the existing HTML distribution, installation, and packaging machinery (see [Build integration](#build-integration)) for free, and appear at `…/en/<version>/llms/…` automatically.

The human-readable documentation should also expose these artifacts through a small, unobtrusive link from `docs/index.rst`. The front page is the right primary location because it is the highest-level public entry point for people, crawlers, and agents that browse ordinary documentation before checking `/llms.txt`. Avoid adding LLM-specific links to every manual page.

## Artifact inventory

Artifacts fall into two categories with different lifecycles:

**Generated** (produced at build time; git-ignored like `html/`, `man/`, and `man-openmpi/man3/bindings/`; shipped in tarballs via the HTML tree copy):

- **`llms.txt`** (versioned): the LLM entry point. A curated Markdown index, not the entire documentation payload.
- **`llms/openmpi-mpi-api.jsonl`**: one JSON object per line describing public MPI API procedures and binding variants.
- **`llms/openmpi-mpi-api.md`**: a single aggregate Markdown corpus for all public MPI APIs.
- **`llms/openmpi-mpi-api-c.md`**: aggregate Markdown focused on the C interface.
- **`llms/openmpi-mpi-api-fortran-mpifh.md`**: aggregate Markdown focused on the `mpif.h` interface.
- **`llms/openmpi-mpi-api-fortran-use-mpi.md`**: aggregate Markdown focused on the `use mpi` interface.
- **`llms/openmpi-mpi-api-fortran-use-mpi-f08.md`**: aggregate Markdown focused on the `use mpi_f08` interface.
- **`llms/man-openmpi/man3/MPI_<name>.3.md`**: per-symbol Markdown pages generated from the corresponding RST manual pages.
- **`llms/man-openmpi/man1/<command>.1.md`**: per-command Markdown pages generated from the section-1 (command) RST manual pages (`mpirun`, `ompi_info`, the wrapper compilers, ...). These document Open MPI commands, not MPI APIs, so they are a Markdown corpus only — there are **no** JSONL catalog records for them.
- **`llms/openmpi-docs-manifest.json`**: a machine-readable inventory of the LLM artifacts in this documentation build.

**Curated** (hand-written or lightly processed; committed to git):

- **`llms/openmpi-mpi-examples.md`**: a curated set of small, validated MPI examples (see [Examples corpus](#examples-corpus)).
- **`llms/openmpi-mpi-interface-guide.md`**: a concise guide for choosing among C, `mpif.h`, `use mpi`, and `use mpi_f08`.
- **`llms/openmpi-mpi-api.schema.json`**: the JSON Schema for the API catalog records.
- **`llms/openmpi-docs-manifest.schema.json`**: the JSON Schema for the manifest.

(The curated source files live under `docs/` in git; the build copies them into the staging directory alongside the generated artifacts so all artifacts share one published location. The two `*.schema.json` files are both published artifacts and the CI validator — the published contract and the check are the same files.)

## `llms.txt`

The top-level `llms.txt` should be short and should link to the artifacts of the documentation version that produced it. On Read the Docs the links are absolute (slug-correct); in a local build they are relative (see the link-strategy reproducibility rule below). The version-neutral `https://docs.open-mpi.org/llms.txt` is served from the Read the Docs *default* version, which may be a series that does not yet carry these artifacts, so `llms.txt` explains the version-slug URL scheme itself rather than relying on the root being authoritative.

Each versioned `llms.txt` should include:

- project name and one-paragraph summary;
- an "about this file" note identifying which build it belongs to — for a Read the Docs `main` build, naming **both** the `main` slug and the `vA.B.x` series it currently represents (dual attribution); for a local build, noting that it is a local, relative-link build;
- the `https://docs.open-mpi.org/en/VERSION_SLUG/` scheme for locating other versions' LLM docs (`main` = the main-branch build, `vA.B.x` = a release branch, `vA.B.C` = a specific tagged release), worded so an LLM that wants a different version can construct the URL, plus a pointer that pre-v5.0.0 documentation lives at the legacy README / FAQ / doc pages. These scheme-documentation URLs are always literal absolute `docs.open-mpi.org` text, even in a local build, so a local consumer still learns where the published versions live;
- a note that the MPI Standard is authoritative for portable MPI semantics;
- a note that `MPIX_*` and `OMPI_*` APIs are Open MPI extensions;
- links to the API catalog, Markdown corpora, examples, and interface guide;
- links to the command (man1) pages and to key human-facing guides such as building, launching, and tuning MPI applications.

## Markdown API corpus

The aggregate Markdown corpora should be generated files derived from the same source RST manual pages used for the human documentation. They should be plain Markdown, with Sphinx-only syntax resolved or removed.

These files should preserve the important content from the manual pages:

- API name and short description;
- syntax for the relevant interface set;
- input, output, and input/output parameter sections;
- description, notes, errors, and see-also links;
- a canonical URL for the human HTML page;
- the source RST path.

They should omit Sphinx navigation, theme chrome, raw unresolved directives, duplicate index material, and generated boilerplate that does not help a tool understand or use the API.

The four language-specific corpora partition the same content by interface. Because `mpif.h` and `use mpi` share signatures, those two corpora will be near-duplicates apart from the access preamble and any interface-specific notes; this is expected and acceptable.

## Per-symbol Markdown pages

The per-symbol Markdown pages should be stripped-down, include-expanded versions of the same manual-page content, not merely raw RST renamed to `.md`. They should match the human manual page semantically, but be normalized for machine consumption.

For example, `MPI_Send.3.md` should contain the `MPI_Send` short description, C and Fortran syntax, parameter descriptions, notes, errors, and see-also links. It should not contain unresolved `.. include::` directives, `:ref:` roles, or Sphinx-only substitution syntax.

Because each documented procedure has its own man page (secondaries `.. include::` the primary), the generator produces one `.md` per page with the include fully expanded, so a tool fetching a secondary page such as `MPI_Ialltoallw.3.md` receives the complete shared content rather than an unresolved include.

## Machine-readable API catalog

`openmpi-mpi-api.jsonl` should contain one JSON object per public MPI API documentation record. Each record should be self-contained enough for a tool to answer signature, parameter, language-interface, and documentation-link questions without scraping Markdown.

The catalog uses a `schema_version` independent of the Open MPI release number, so the JSON schema can evolve while the Open MPI release series remains the same.

Required top-level fields:

- **`schema_version`**: version of the JSONL record schema, such as `1`.
- **`project`**: always `open-mpi` for this catalog.
- **`docset`**: object describing the documentation build. It contains the Open MPI version derived from the top-level `VERSION` file and the `schema_version`. It does **not** carry build-identity fields (see [Build identity and reproducibility](#build-identity-and-reproducibility)); those live only in the manifest.
- **`name`**: primary API name, such as `MPI_Send`.
- **`kind`**: one of `standard`, `extension`, `deprecated`, or `removed`.
- **`standard`**: object describing MPI Standard version information when known.
- **`category`**: API family such as `point-to-point`, `collective`, `communicator`, `datatype`, `rma`, `io`, `sessions`, or `tools`.
- **`languages`**: the set of interfaces for which this record provides bindings — exactly the distinct `language` values across `bindings` (a subset of `c`, `fortran_mpifh`, `fortran_use_mpi`, and `fortran_mpi_f08`). For a fully-generated standard-API record this is all four; a partial record lists only what it carries.
- **`bindings`**: array of binding variants. Each variant includes language, procedure name, rendered signature, parameters, return convention, and whether it is a large-count variant.
- **`parameters`**: normalized parameter metadata where it can be obtained reliably: name, direction, type, intent, optionality, and description. For extension records, structured fields may be `unknown`.
- **`semantics`**: object containing optional fields such as `blocking`, `collective`, `local`, `initialization_required`, and `threading_notes` when known.
- **`errors`**: object describing the error-return convention and a link to the human error-handling documentation.
- **`seealso`**: array of related API names.
- **`documented_with`**: array of co-documented procedure names when the API shares a man page with others (empty or absent otherwise).
- **`urls`**: object containing canonical HTML and Markdown URLs for **this** procedure's own page.
- **`sources`**: object containing source RST paths and generated binding metadata sources.

Every field above is always present in a record (so consumers need not handle missing keys), with one exception and one refinement: `documented_with` may be omitted when empty, and the when-known fields (`standard`, `category`, `parameters`, `semantics`, `errors`) are set to `null` or an empty array for best-effort/extension records rather than dropped. This is exactly the `required` set of the published JSON Schema, so the prose list and the machine-checkable contract agree.

A record is therefore locatable from its own `urls` and `docset.ompi_version` alone; build provenance (which commit produced it, when) is intentionally **not** embedded per-record but is available once per build in the manifest. This is the deliberate trade-off that keeps per-artifact content hashes a function of semantic content only.

The catalog records conform to a published JSON Schema, `openmpi-mpi-api.schema.json` (and the manifest to `openmpi-docs-manifest.schema.json`). These schema files are authored as the formal contract, published as artifacts, and reused directly by CI as the validator, so the published structural contract and the check can never drift apart. Cross-field invariants that plain JSON Schema cannot express — for example, that a record's `languages` equals the distinct set of its `bindings[].language` — are enforced by additional CI checks layered on top of schema validation. The JSON Schema is versioned in step with `schema_version` / `artifact_schema_version`.

The spec should ship sample records (in this `specs/` directory or a fixture) for `MPI_Init`, `MPI_Send`, `MPI_Allreduce`, `MPI_Alltoallw` (multi-procedure, large-count), and one `MPIX_*` extension, and CI should check that the samples validate against the JSON Schema.

## Build identity and reproducibility

Open MPI is already a reproducible-builds-aware project: `config/getdate.sh` honors `SOURCE_DATE_EPOCH`, the release notes instruct users to set `SOURCE_DATE_EPOCH`/`USER`/`HOSTNAME` for reproducible builds, and `config/opal_get_version.sh` derives the repo revision from `git describe --tags --always` (falling back to a date when there is no `.git`). Sphinx's own `html_last_updated_fmt` footer also honors `SOURCE_DATE_EPOCH`. The LLM artifacts must fit this existing model rather than invent a new one.

### Versioning

The Open MPI version in each record and in the manifest must be derived from the top-level `VERSION` file, following the same rules already used by `docs/conf.py`. When building on Read the Docs, the published documentation path may use the Read the Docs version slug, but the Open MPI release metadata still comes from `VERSION`.

### Build-identity fields are manifest-only

The following fields change between builds and are written **only in the manifest** — never inside the catalog records, Markdown corpora, or per-symbol pages:

- **`git_commit`**: the source commit used for the documentation build, when available.
- **`git_describe`**: `git describe --tags --always` output, when available.
- **`generated_at`**: UTC timestamp for the artifact generation.

Rationale: if `git_commit` were embedded in every catalog record, then every artifact's content hash would change on every commit to the repository — even a commit that touches no documentation — which would make the manifest's per-artifact hashes useless as change detectors. By keeping build identity in the manifest only, each artifact's hash is a pure function of its semantic content, so "hash changed ⇔ documentation content changed." (`schema_version` and `artifact_schema_version` are treated as semantic: a schema bump legitimately changes everything.)

### Reproducibility rules

- **Timestamp.** `generated_at` is derived from `SOURCE_DATE_EPOCH` when set (UTC), exactly as `getdate.sh` and Sphinx's `format_date` do; otherwise it falls back to the build commit date, then to wall-clock time. CI and `make dist` set/derive `SOURCE_DATE_EPOCH`.
- **Semantic artifacts are wall-clock-free.** The catalog, corpora, and per-symbol pages contain no timestamp, so they are byte-identical across reruns at a given commit, regardless of `SOURCE_DATE_EPOCH`.
- **Content hashing.** Manifest content hashes are computed over the (wall-clock-free) artifact bytes.
- **Stable ordering.** All collections (records, parameters, see-also, file listings) are emitted in a deterministic order.
- **Link strategy: local builds are relative, Read the Docs builds are absolute.** A local build from git produces the artifacts for a *local* LLM/tool that reads the files straight off disk; there is no docs.open-mpi.org site in play, so the RTD URL scheme is irrelevant and every generated link — in `llms.txt`, the JSONL record `urls`, the manifest `url`s, and the per-symbol/man1 page Canonical-HTML headers — is made **relative to the file that contains it**, yielding a self-contained, portable tree. A Read the Docs build publishes under `https://docs.open-mpi.org/en/<slug>/`, so there every link is **absolute** and uses the published version slug (`…/en/main/…` for the `main` branch, `…/en/v6.1.x/…` for a release branch, `…/en/v6.1.0/…` for a tagged release). The base is resolved in this order: an explicit `--url-base`/`OMPI_LLM_URL_BASE` override (absolute); then `READTHEDOCS_CANONICAL_URL` / `READTHEDOCS_VERSION` (absolute, slug-correct); otherwise relative (local). Determinism is "same source + same environment ⇒ no diff"; the determinism check runs both passes in one environment, so the strategy and slug are constant across them. Exception: the version-slug *scheme-documentation* URLs printed inside `llms.txt` are always literal absolute `docs.open-mpi.org` text, in every build, so a local consumer still learns where the published versions live.
- **Graceful degradation.** In a from-tarball build with no `.git` and no `SOURCE_DATE_EPOCH`, the generator must not fail: git fields become `unknown`/omitted and `generated_at` falls back to the tarball/build date, mirroring `opal_get_version.sh`.

A determinism check (see [Build integration](#build-integration)) reruns the generator at a fixed `SOURCE_DATE_EPOCH` and commit and asserts no file diffs.

### Reproducible human-docs fix (folded in)

While adopting `SOURCE_DATE_EPOCH`, this effort also closes a small pre-existing reproducibility gap in the human docs build: `docs/conf.py` currently computes the copyright year with `datetime.datetime.now().year`, which ignores `SOURCE_DATE_EPOCH`. This makes the copyright string and the `|year|` RST substitution non-reproducible even on the documented reproducible-build path. The fix derives `year` from `SOURCE_DATE_EPOCH` when set, falling back to `datetime.now()` otherwise. As part of this, verify that the Sphinx "Last updated on:" HTML footer is reproducible under `SOURCE_DATE_EPOCH` (it is, via `format_date`) and check the man-page `.TH` date for the same property. The result is a docs build (HTML, man, and LLM artifacts) that is fully reproducible under a single, already-documented knob.

## Chunk manifest

`openmpi-docs-manifest.json` is an inventory for retrieval systems. It lets consumers decide what to fetch, cache, or refresh without guessing from filenames. It is the single place that carries build identity.

The manifest should include:

- manifest schema version (`artifact_schema_version`);
- Open MPI version derived from `VERSION`;
- Read the Docs version slug when available;
- `git_commit`, `git_describe`, and `generated_at` (the only build-identity fields in any artifact);
- one entry per generated and curated artifact, **except the manifest itself**;
- URL, local path, media type, content hash, byte size, and estimated token count for each artifact;
- symbol names covered by each artifact, when applicable;
- language/interface set covered by each artifact, when applicable.

The manifest does not inventory itself: a file cannot record its own content hash and byte size (writing them changes both), and the manifest is the one artifact that is intentionally not wall-clock-free. Every other artifact — including the versioned `llms.txt` — is listed. For this to hold, the versioned `llms.txt` header must itself be timestamp-free (it carries the Open MPI version, not a generation time); build provenance for the whole set lives once, in the manifest.

The manifest does not require an independently incremented documentation release number. Instead, consumers compare manifest identity fields and artifact content hashes. For example, if the `v6.1.x` branch receives a documentation fix before a `v6.1.1` release, the Open MPI release value may remain unchanged, but the manifest's git identity and the hashes of the *affected* artifacts will change, while unaffected artifacts' hashes stay the same.

## Shared generator infrastructure

The implementation should share infrastructure with `docs/generate-mpi-man3-bindings.py` rather than creating unrelated parsers.

At minimum, the shared infrastructure should provide:

- parsing of MPI manual-page directives such as `.. mpi-bindings:` (a single-colon comment line, parsed textually, not a registered Sphinx directive);
- loading of `pympistandard` (from the embedded `3rd-party/pympistandard/src`) and `mpi-standard-apis.json`;
- generation of C and Fortran binding strings (C `iso_c`, fixed-form `f90`, and `f08`, plus large-count "embiggened" variants);
- a separate path for extracting extension signatures from hand-written RST `SYNTAX` blocks;
- parsing of the top-level `VERSION` file;
- build-identity helpers for `git_commit` / `git describe --tags --always` and `SOURCE_DATE_EPOCH`-aware timestamps;
- common handling for primary API pages that document multiple MPI procedures.

One possible implementation is to extract common code into a helper module under `docs/` and have both `generate-mpi-man3-bindings.py` and the LLM artifact generator import it. The helper must remain compatible with Open MPI's supported Python version floor, which is **3.6** (per `python_min_version` in `VERSION`). Avoid importing `docs/conf.py` directly for version parsing if doing so would trigger Sphinx configuration side effects; factor the `VERSION` parser so both can share it without importing Sphinx config.

## Examples corpus

`openmpi-mpi-examples.md` is a curated artifact. Its purpose is representative coverage, not exhaustive API coverage.

To honor the "avoid a second hand-maintained reference" goal, **reuse the existing top-level `examples/` directory where it already covers a case**, rather than authoring parallel copies. `examples/` already contains:

- `hello_*` — minimal initialize/finalize, in C, `mpif.h` (`hello_mpifh.f`), `use mpi` (`hello_usempi.f90`), and `use mpi_f08` (`hello_usempif08.f90`);
- `ring_*` — point-to-point send/receive, in the same four interfaces;
- an `examples/Makefile` that already compiles them with the Open MPI wrapper compilers.

Recommended approach: source/generate the Markdown for the covered cases from those files, and wire CI compilation through the existing `examples/Makefile` (which gets "compile in CI" nearly for free). Hand-author only the genuine gaps:

- collective communication;
- nonblocking communication;
- derived datatypes;
- communicators;
- MPI-IO;
- RMA.

The corpus must include at least one example for each supported Fortran interface set (the reused `hello_*`/`ring_*` files already provide this). Examples should use the Open MPI wrapper compilers (`mpicc`, `mpifort`). Where practical, examples should be compiled or otherwise validated in CI; the spec should record which examples are compiled and which are documentation-only.

## Interface guide

`openmpi-mpi-interface-guide.md` is a concise guide that helps tools choose the correct interface for generated code. It should state:

- prefer `use mpi_f08` for new Fortran code when available;
- `mpif.h` and `use mpi` exist for legacy code, and share the same procedure signatures (differing only in `INCLUDE 'mpif.h'` vs `USE mpi`);
- C large-count variants use `_c` names where provided by MPI;
- `MPIX_*` and `OMPI_*` APIs are Open MPI extensions and are not portable MPI Standard APIs;
- applications should normally use `mpicc`, `mpicxx`, and `mpifort` rather than invoking the underlying compiler and linker flags manually.

## Build integration

Generated artifacts are created as part of the documentation build. The generator writes them (and copies the curated sources) into a build-tree staging directory (`$(builddir)/llms-build/`). It is run in **both** build paths, mirroring how `generate-mpi-man3-bindings.py` is run: for `make` builds, from a sentinel target in `BUILT_SOURCES` (after the man3 bindings, before Sphinx); for Read the Docs builds — which run `sphinx-build` directly and never run `make` — from `.readthedocs-pre-create-environment.sh`. A `build-finished` hook in `conf.py` then copies the staging tree (`llms-build/llms` → `<output>/llms`, `llms-build/llms.txt` → `<output>/llms.txt`) into the Sphinx HTML output, replacing any prior copy so it matches the staged tree. Because the copy lives in the Sphinx hook, it happens under `make` and on Read the Docs alike, and the artifacts are published with the rest of the versioned documentation.

Because the artifacts ride inside the HTML output tree, they inherit the existing HTML machinery:

- the `html-local` target already copies `_build/html` into `docs/html/`, which is in `EXTRA_DIST`, so the artifacts ship in release tarballs automatically;
- `install-data-hook` already installs the entire `html` tree under `$(docdir)`, so the artifacts install with no new install rule;
- `uninstall-hook` already removes `$(docdir)` recursively, so no new uninstall rule is needed.

This means the bulk of distribution/installation work is already handled; the new Automake work is mostly: invoking the generator, declaring its source/build directories (with VPATH-safe output to the build tree), declaring curated source files in `EXTRA_DIST`, and ensuring `make distcheck` passes.

Open MPI distribution tarballs already include pre-rendered HTML and man pages so end users do not need Sphinx installed. The LLM artifacts follow the same convention: a tarball ships the already-rendered `llms/` tree inside `html/`. If `configure` did not find Sphinx, or if the rendered artifacts are already present and do not need regeneration, `make` must not require Sphinx merely to preserve, install, or package the pre-rendered LLM artifacts. In a developer Git clone without usable pre-rendered artifacts, generating or refreshing them may require the same Sphinx/documentation-tooling prerequisites as the rest of the docs build, and should fail with a clear message when that tooling is unavailable.

The human-facing docs should include either a one-sentence entry in the `docs/index.rst` documentation-locations area, or a short `docs/llm-friendly-docs.rst` landing page linked from `docs/index.rst` if more explanation is needed. The landing page, if added, should link to `/llms.txt`, the versioned `llms.txt`, the generated API catalog, the Markdown corpora, and the manifest.

The generated output is deterministic except for the explicitly documented manifest-only build-identity fields. CI verifies that generated artifacts are up to date by rerunning the generator (at a pinned `SOURCE_DATE_EPOCH`, commit, and URL base) and asserting no file diffs.

## Validation expectations

CI should validate, at minimum: that each catalog record validates against the published `openmpi-mpi-api.schema.json` and the manifest against `openmpi-docs-manifest.schema.json` (this covers JSONL syntax, required fields, and allowed enum values such as `project`, `kind`, `category`, and `languages`); that each record's `languages` equals the distinct set of its `bindings[].language` (a cross-field invariant beyond JSON Schema); that every record links back to the human documentation; that every Markdown file is free of unresolved `.. include::` directives, `:ref:` roles, and Sphinx-only substitutions; that manifest hashes, byte sizes, and entry coverage match the artifacts; that every artifact referenced by `llms.txt` exists; that the sample records validate against the JSON Schema; and the determinism (no-diff) rerun.

## Release and maintenance process

### Schema evolution and compatibility

- **Additive within a `schema_version`.** New fields must be optional; existing consumers must keep working. A field may be marked deprecated (but is still emitted) for one Open MPI release series before it is removed. Never remove, rename, repurpose, or change the meaning of an existing field without a version bump. So that an additive field does not break a consumer validating against an older cached copy of the schema, the published JSON Schemas use open objects (they do not set `additionalProperties: false`); an unknown future field is tolerated rather than rejected.
- **Bump on breaking change only.** `schema_version` (catalog) and `artifact_schema_version` (manifest) increment only when a non-additive change lands — not on every release. The decision is owned by the docs maintainers and recorded in the schema documentation that ships with the catalog.

### Changelog and release notes

- A `schema_version`/`artifact_schema_version` bump or a notable field change gets an entry in `docs/release-notes/changelog/vMAJOR.MINOR.x.rst`.
- The artifacts themselves get a **single** release note when first shipped (they are user-visible in installed documentation).
- Routine regeneration — content that flows automatically from changed RST or metadata — does **not** get changelog entries; consumers detect those changes via the manifest's identity fields and per-artifact hashes.

### Keeping curated docs in sync

Generated artifacts regenerate automatically from RST/metadata, so they cannot drift. The curated docs (interface guide, examples) can. The expectation — documented in both the developer docs and `AGENTS.md` — is that a pull request changing public MPI documentation should also update the affected curated LLM docs when relevant (for example, adding an example or a guide note for a newly documented API). A lightweight CI **drift hint** (not a hard gate) for likely gaps — such as a new man page with no catalog coverage, or an interface-guide/example reference to a removed symbol — is envisioned as follow-on work but is **not yet implemented**. The current validator checks that the manifest inventories every on-disk artifact, but does not check procedure-to-record coverage or curated-symbol references, so such gaps are currently silent.

### Release-time regeneration

Release managers already set `SOURCE_DATE_EPOCH` (and `USER`/`HOSTNAME`) when producing reproducible release tarballs. Because the generator honors `SOURCE_DATE_EPOCH` automatically, **no separate regeneration step is required** of the release manager; the LLM artifacts are produced by the normal docs build. This dependency is documented in `docs/developers/llm-friendly-docs.rst`.

### Verifying publication

After a merge that affects the artifacts, a documented manual spot-check confirms publication: fetch the merged version's own `llms.txt` (for example `https://docs.open-mpi.org/en/main/llms.txt` for the `main` branch) and one versioned artifact (for example the manifest) and confirm they resolve and parse. The version-neutral `https://docs.open-mpi.org/llms.txt` follows Read the Docs' default version, which may not yet carry these artifacts, so the spot-check targets the specific version slug just published. No new publication infrastructure is added.

## Follow-on work

- Extend the same artifact model to OpenSHMEM public APIs (adding OpenSHMEM-specific schema examples first) — **deferred until there is concrete demand**.

(Earlier candidates that were explicitly considered and dropped: an `llms-full.txt` full-payload entry point, compressed downloadable bundles, and stable redirects for renamed artifacts. The aggregate corpora, release tarballs/manifest, and ad hoc Read the Docs redirects respectively make these unnecessary. JSON Schema files were promoted into current scope.)
