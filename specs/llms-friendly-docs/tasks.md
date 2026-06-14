# LLM-friendly documentation task list

This task list tracks implementation of the LLM-friendly documentation artifacts described in [spec.md](spec.md).

Locked decisions (see spec for rationale):

- **Fortran model:** four language tags/corpora (`c`, `fortran_mpifh`, `fortran_use_mpi`, `fortran_mpi_f08`); `mpif.h` and `use mpi` share the `f90` signature and differ only in the access preamble.
- **Extensions:** include both `MPIX_*` and `OMPI_*`; best-effort fidelity (verbatim signature text, `unknown` structured params when not extractable).
- **Determinism:** honor `SOURCE_DATE_EPOCH`; build-identity fields (`generated_at`, `git_commit`, `git_describe`) live in the manifest **only**; semantic artifacts are wall-clock-free; content hashes are over semantic bytes.
- **Storage:** generated artifacts are git-ignored and ride the HTML output tree (`_build/html/llms/`); curated sources (interface guide, examples) are committed.

## Phase 0: repository and build-system survey

- [x] Inventory the existing MPI man-page source files under `docs/man-openmpi/man3/` (612 MPI man pages listed in `OMPI_MAN3`).
- [x] Inventory the generated MPI binding include files under `docs/man-openmpi/man3/bindings/` (git-ignored, generated to the build tree).
- [x] Inventory existing man pages that document multiple MPI procedures via `.. mpi-bindings:` (57 directives).
- [x] Inventory the extension man pages (`MPIX_*`, `OMPI_Affinity_str`) and confirm they are absent from `pympistandard` (11 `MPIX_*` + `OMPI_Affinity_str`; signatures only in hand-written RST).
- [x] Inventory the secondary-procedure stub pages (e.g. `MPI_Ialltoallw.3.rst`) that `.. include::` a primary page body (`:start-after: .. include_body`).
- [x] Inventory overview/non-procedure man pages (e.g. `MPI_T.3`, `Open-MPI.7`, `MPI_Errors.3`) that will get Markdown but no JSONL record.
- [x] Identify all current documentation build modes in `docs/Makefile.am`: developer tree with Sphinx (`OPAL_BUILD_DOCS`), release tarball with pre-rendered docs, and build without Sphinx.
- [x] Confirm the existing HTML lifecycle that the artifacts will ride: `html-local` copy into `docs/html/`, `EXTRA_DIST` inclusion, `install-data-hook`, `uninstall-hook`.
- [x] Decide where generated LLM artifacts live: git-ignored, written to a build-tree staging directory and copied into `_build/html/llms/` by a `conf.py` `build-finished` hook (not `html_extra_path` — see Phase 5), shipped via the HTML tree copy. Curated sources committed under `docs/llms-src/`. *(Decided.)*

## Phase 1: schema and format design

- [x] Define `openmpi-mpi-api.jsonl` schema version 1.
- [x] Define the `docset` object as **semantic-only**: Open MPI version (from `VERSION`) and `schema_version`. Explicitly exclude build-identity fields (those live in the manifest).
- [x] Define `project` as the literal value `open-mpi`.
- [x] Define allowed `kind` values and rules for assigning `standard`, `extension`, `deprecated`, and `removed`.
- [x] Define allowed `category` values and rules for assigning APIs to categories.
- [x] Define allowed `languages` values: `c`, `fortran_mpifh`, `fortran_use_mpi`, and `fortran_mpi_f08`.
- [x] Define the `bindings` object shape, including language, procedure name, signature text, large-count marker, return convention, and parameter list. Note that `fortran_mpifh` and `fortran_use_mpi` share the `f90` signature, differing only in preamble.
- [x] Define the normalized `parameters` object shape (name, direction, datatype, intent, optionality, human description) and the **source-of-truth rule**: structured fields from `mpi-standard-apis.json`/`pympistandard`; human description from RST `INPUT/OUTPUT PARAMETERS` bullets, falling back to JSON `desc`.
- [x] Define how extension records represent unreliable structured fields (`unknown` / omitted) while preserving verbatim signature text.
- [x] Define the `semantics` object fields that can be filled reliably from existing documentation and metadata.
- [x] Define URL fields for canonical HTML, per-symbol Markdown, aggregate Markdown, and source RST paths.
- [x] Define the `documented_with` field for procedures that share a man page.
- [x] Define how to represent man pages that document multiple related procedures (one record per procedure; own URLs; `documented_with` cross-links).
- [x] Define how to represent large-count `_c` variants and Fortran generic interfaces.
- [x] Define how to represent Open MPI extension symbols (`MPIX_*`, `OMPI_*`).
- [x] Define how missing or uncertain metadata should be omitted, set to `null`, or marked as `unknown`.
- [x] Create sample JSONL records for `MPI_Init`, `MPI_Send`, `MPI_Allreduce`, `MPI_Alltoallw`, and one `MPIX_*` extension (`specs/llms-friendly-docs/sample-records.jsonl`; validated against the schema).
- [x] Define the per-symbol Markdown page structure (handles **arbitrary** section headings, not a fixed set).
- [x] Define the aggregate Markdown structure for all APIs.
- [x] Define the aggregate Markdown structure for each language/interface-specific corpus.
- [x] Define the manifest JSON schema, including the manifest-only build-identity fields and per-artifact metadata.
- [x] Author the published JSON Schema files — `openmpi-mpi-api.schema.json` (catalog) and `openmpi-docs-manifest.schema.json` (manifest) — versioned with `schema_version`/`artifact_schema_version`, to serve as both the formal contract and the CI validator.
- [x] Define content-hash, byte-size, and token-estimate rules for manifest entries (hash computed over wall-clock-free semantic bytes).
- [x] Define how `generated_at` is handled: `SOURCE_DATE_EPOCH`-derived, manifest-only, semantic artifacts wall-clock-free. *(Decided.)*

## Phase 2: shared MPI metadata infrastructure

- [x] Extract common helper code from `docs/generate-mpi-man3-bindings.py` where practical (new `docs/ompi_docs_common.py`).
- [x] Preserve `docs/generate-mpi-man3-bindings.py` behavior while moving shared logic (byte-identical bindings output verified via before/after diff; full clean docs build succeeds).
- [x] Add shared parsing for `.. mpi-bindings:` directives (single-colon comment, parsed textually).
- [x] Add shared discovery of primary MPI man pages and secondary MPI procedures documented on those pages.
- [x] Add shared loading of `pympistandard` from the embedded `3rd-party/pympistandard/src` tree.
- [x] Add shared loading of `docs/mpi-standard-apis.json`.
- [x] Add shared rendering of C binding strings (`iso_c`).
- [x] Add shared rendering of `mpif.h` / `use mpi` binding strings (the shared `f90` string + per-interface preamble).
- [x] Add shared rendering of `use mpi_f08` binding strings (`f08`).
- [x] Add shared detection of large-count ("embiggened") binding variants.
- [~] Add a shared extension-signature extractor that reads hand-written RST `SYNTAX` code blocks for `MPIX_*`/`OMPI_*` pages. *(Deferred to Phase 3, where the RST resolver is built and the exact shape is known.)*
- [x] Add shared parsing of the top-level `VERSION` file (without importing `docs/conf.py` / triggering Sphinx side effects).
- [x] Add shared build-identity helpers: `git_commit`, `git describe --tags --always`, and a `SOURCE_DATE_EPOCH`-aware UTC timestamp (with graceful no-`.git`/no-epoch fallback, mirroring `opal_get_version.sh`).
- [x] Keep all new Python compatible with the Python 3.6 floor (`python_min_version` in `VERSION`); the module uses `str.format` and standard-library APIs available in 3.6.
- [~] Add focused tests or fixtures for shared metadata helpers where practical. *(Smoke-tested; the formal test harness is added in Phase 7.)*

## Phase 3: LLM artifact generator

- [x] Add a generator script for LLM-friendly docs (`docs/generate-llm-docs.py`).
- [x] Give the generator explicit `--srcdir`, `--builddir`, and output-directory options consistent with existing docs generators.
- [x] Add a `--url-base` option (or environment-derived setting) for the public documentation URL base; default to the `VERSION`-derived series path so URLs are stable across local and Read the Docs builds.
- [x] Generate `llms/openmpi-mpi-api.jsonl` (standard records metadata-driven; extension records best-effort from RST) — 512 records (501 standard + 11 extension), all schema-valid.
- [x] Generate per-symbol Markdown under `llms/man-openmpi/man3/` (one per man page; includes fully expanded) — 525 pages.
- [x] Generate `llms/openmpi-mpi-api.md`.
- [x] Generate `llms/openmpi-mpi-api-c.md`.
- [x] Generate `llms/openmpi-mpi-api-fortran-mpifh.md`.
- [x] Generate `llms/openmpi-mpi-api-fortran-use-mpi.md`.
- [x] Generate `llms/openmpi-mpi-api-fortran-use-mpi-f08.md`.
- [x] Generate `llms/openmpi-docs-manifest.json` (the only artifact carrying build identity).
- [x] Copy curated source artifacts (interface guide, examples, and the two `*.schema.json` files) into the output `llms/` tree so all artifacts share one location (copy mechanism in place; schemas present now, guide/examples land in Phase 4).
- [x] Generate a versioned `llms.txt` for the current documentation build.
- [~] Generate or stage a top-level `llms.txt` suitable for the Read the Docs default version. *(The versioned `llms.txt` at the staging root is what RTD serves at the version root and, for the default version, at `/`. A `/llms.txt` redirect is an available RTD-side option if needed — see Phase 5.)*
- [x] Resolve `.. include::` directives needed by per-symbol Markdown pages (including secondary-page includes of primary bodies).
- [x] Resolve common Sphinx roles such as `:ref:` into readable Markdown links or plain text (including multi-line roles and RST hyperlinks).
- [x] Resolve common Sphinx substitutions used in MPI man pages.
- [x] Handle arbitrary RST section headings, not just a fixed set.
- [x] Preserve code blocks with correct language tags where possible.
- [x] Preserve see-also relationships as symbol names and links.
- [x] Exclude Sphinx navigation, theme content, duplicate index content, and unresolved directives from generated Markdown (verified: zero unresolved `:ref:`/include/directive/substitution/hyperlink across all 525 pages).
- [x] Keep semantic artifacts wall-clock-free; write build identity only in the manifest (verified deterministic: byte-identical reruns at fixed `SOURCE_DATE_EPOCH`).
- [x] Ensure generated files are written only when content changes.
- [x] Ensure generated output ordering is stable across runs.
- [x] Emit make-style status messages consistent with existing docs generators.
- [x] Add the shared extension-signature extractor for `MPIX_*`/`OMPI_*` pages (`extension_bindings_from_rst`; deferred from Phase 2).

## Phase 4: curated source artifacts

- [x] Write `llms/openmpi-mpi-interface-guide.md` (committed source `docs/llms-src/openmpi-mpi-interface-guide.md`).
- [x] Explain when generated code should choose C.
- [x] Explain when generated code should choose `use mpi_f08`.
- [x] Explain legacy status and shared-signature relationship of `mpif.h` and `use mpi`.
- [x] Explain wrapper compiler usage: `mpicc`, `mpicxx`, and `mpifort`.
- [x] Explain that `MPIX_*` and `OMPI_*` symbols are Open MPI extensions, not portable MPI Standard APIs.
- [x] Explain large-count `_c` variants and when to use them.
- [x] Write `llms/openmpi-mpi-examples.md` (committed source `docs/llms-src/openmpi-mpi-examples.md`).
- [x] Reuse the existing top-level `examples/` files for covered cases (hello = init/finalize; ring = point-to-point) across C, `mpif.h`, `use mpi`, and `use mpi_f08`, rather than authoring parallel copies.
- [x] Hand-author the gap examples: collective, nonblocking, derived datatype, communicator, MPI-IO, and RMA.
- [x] Ensure at least one example per supported Fortran interface set (covered by the reused `hello_*`/`ring_*` files).
- [x] Include compile commands using Open MPI wrapper compilers.
- [x] Decide and record which examples are compiled in CI: the canonical `examples/` `hello_*`/`ring_*` programs are compiled via `examples/Makefile`; the additional C examples in the corpus are compile-checked (verified here with `mpicc -Wall`, all 8 C blocks build) and documented as documentation examples.

## Phase 5: Sphinx, Automake, and Read the Docs integration

- [x] Have the generator write artifacts (and copy curated sources) into a build-tree staging directory (`$(builddir)/llms-build/`). *(Published by a `build-finished` hook in `conf.py` that copies the staging tree into the Sphinx HTML output — NOT `html_extra_path` (which `exclude_patterns` would suppress) and NOT a Makefile step (Read the Docs never runs `make`). The generator runs from the Makefile sentinel for `make` builds and from `.readthedocs-pre-create-environment.sh` for RTD; the conf.py hook publishes in both.)*
- [x] Add the generator to `docs/Makefile.am` with correct source/build directory handling and a sentinel target (`SENTINEL_OMPI_LLM`, a dep of `$(ALL_MAN_BUILT)` in `BUILT_SOURCES`) that runs the generator **after** the man3 bindings and **before** Sphinx, mirroring `SENTINEL_OMPI_MAN3_BINDING`.
- [x] Ensure VPATH builds write generated LLM artifacts to the build-tree staging directory, not the source tree (generator `--builddir`; outdir defaults to `$(builddir)/llms-build`).
- [x] Add curated source artifacts (interface guide, examples, and the `*.schema.json` files) and the generator script (`generate-llm-docs.py`, `ompi_docs_common.py`) to `EXTRA_DIST`.
- [x] Add `llms-build/` build output to `.gitignore` (alongside `html`, `man`, `man-openmpi/man3/bindings`) and to the `clean-local`/`maintainer-clean-local` rules.
- [x] Ensure developer-tree builds regenerate LLM artifacts when source RST, binding metadata, generator code, or curated source artifacts change (sentinel prerequisites).
- [x] Ensure developer-tree builds avoid regenerating LLM artifacts when output is already current (write-only-when-changed + sentinel mtime).
- [x] Confirm rendered LLM artifacts are included in tarballs via the existing `html-local` copy (verified: `docs/html/llms/` populated after `make html-local`).
- [~] Run `make dist` and validate that the generated LLM artifacts plus the curated sources are present inside the distribution tarball under the docs `html/` tree. *(Validated via the docs `make distdir`: it packages `html/llms.txt`, 532 `.md` artifacts, `llms-src/` sources, and the generator scripts. The full-tree `make dist` cannot complete in this `--disable-mpi-fortran` tree due to an unrelated missing Fortran generated header — `ompi/mpi/fortran/use-mpi-ignore-tkr`, not docs.)*
- [x] Confirm release-tarball builds preserve/install/package pre-rendered LLM artifacts when Sphinx is unavailable, via the existing HTML install path (the artifacts ride inside `html/`, which the no-Sphinx path installs from the pre-rendered tree).
- [~] Ensure missing pre-rendered LLM artifacts in a Git clone fail with a clear message when required tooling is unavailable. *(Inherited from the existing HTML/man no-Sphinx behavior; not separately exercised.)*
- [x] Confirm LLM artifacts install under `$(docdir)` via the existing `install-data-hook` (no new install rule — it copies the entire `html` tree, which now contains `llms/`).
- [x] Confirm `uninstall-hook` already removes installed LLM artifacts (no new uninstall rule — it removes `$(docdir)` recursively).
- [x] Add `make distcheck`-safe handling for generated LLM artifacts and copied build-tree artifacts (`clean-local`/`distclean-local`/`maintainer-clean-local` remove `llms-build` and the copied `html`/`man`).
- [~] Run `make distcheck` end to end with the LLM artifacts present (`DISTCHECK_CONFIGURE_FLAGS`, `venv/bin` on `PATH`, `AM_MAKEFLAGS="-j32"`). *(Blocked in this tree: `make distcheck` first runs `make dist`, which fails on the unrelated `--disable-mpi-fortran` Fortran-subdir issue above. The documented command remains for a Fortran-enabled CI environment.)*

      PATH="$PWD/venv/bin:$PATH" make distcheck \
        DISTCHECK_CONFIGURE_FLAGS="--disable-mpi-fortran --enable-sphinx --with-hwloc=internal --with-pmix=internal --with-prrte=internal --with-libevent=internal PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig:" \
        AM_MAKEFLAGS="-j32"
- [x] Publish a versioned `llms.txt` for each documentation version (generated per build at the HTML version root).
- [~] Publish a top-level `llms.txt` from the Read the Docs default version. *(RTD serves the default version's own self-describing `llms.txt` at `/`; there is no separate top-level index. A `/llms.txt` → versioned-path redirect is an available RTD-side option if different root behavior is ever needed.)*
- [x] Add a concise human-facing link from `docs/index.rst` to the LLM-friendly documentation entry point.
- [x] Add a short `docs/llm-friendly-docs.rst` landing page only if the front-page link needs more explanation than one sentence. *(Decision: the one-sentence `index.rst` entry suffices; no separate landing page added.)*
- [x] Verify the local HTML build contains the generated `llms/` tree (`_build/html/llms/`, 525 per-symbol pages + corpora + manifest).
- [x] Verify the local HTML build contains the versioned `llms.txt` (`_build/html/llms.txt`).
- [~] Verify Read the Docs serves `/llms.txt` from the default version. *(Implementation wired for RTD: the generator runs in `.readthedocs-pre-create-environment.sh`, a `conf.py` `build-finished` hook copies the artifacts into the HTML output, and URLs use the RTD slug via `READTHEDOCS_CANONICAL_URL` (verified locally by simulating the RTD env → `…/en/main/…`). Live confirmation is still a post-merge RTD check — documented in Phase 8.)*
- [~] Verify Read the Docs serves versioned LLM artifacts from versioned documentation URLs. *(Same RTD wiring as above; live confirmation is post-merge.)*

## Phase 6: reproducible docs build (folded-in fix)

- [x] Make `docs/conf.py` derive the copyright `year` from `SOURCE_DATE_EPOCH` when set, falling back to `datetime.now()` (verified: SDE=1700000000 -> 2023; unset -> current year).
- [x] Verify the Sphinx "Last updated on:" HTML footer is reproducible under `SOURCE_DATE_EPOCH` (it is, via `format_date` in `sphinx/builders/html`).
- [x] Check the nroff man-page `.TH` date for the same `SOURCE_DATE_EPOCH` reproducibility property and fix if needed (no fix needed: `sphinx/writers/manpage.py` sets the date via `format_date`, which honors `SOURCE_DATE_EPOCH`).
- [~] Confirm the full docs build (HTML, man, LLM artifacts) is reproducible under a single `SOURCE_DATE_EPOCH` setting. *(Verified component-wise: copyright year, HTML footer, man `.TH` date, and the LLM artifacts all derive from `SOURCE_DATE_EPOCH`; the full two-build no-diff confirmation is the Phase 7 determinism check.)*

## Phase 7: validation and CI

All of the following are implemented in `docs/validate-llm-docs.py` and run via
`make check` (a `check-local` target), which also performs the determinism
rerun. `jsonschema` is added to `docs/requirements.txt`; the validator degrades
gracefully (non-schema checks still run) if it is unavailable.

- [x] Validate every JSONL record against the published `openmpi-mpi-api.schema.json` (covers JSONL syntax, required fields, and allowed enum values such as `project`, `kind`, `category`, and `languages`).
- [x] Validate `openmpi-docs-manifest.json` against the published `openmpi-docs-manifest.schema.json`.
- [x] Validate the cross-field invariant (beyond JSON Schema) that each record's `languages` equals the distinct set of its `bindings[].language`.
- [x] Add validation that every JSONL record has at least one URL back to the human documentation.
- [x] Add validation that the `docset` object contains no build-identity fields.
- [x] Add validation that manifest hashes match generated artifact content.
- [x] Add validation that manifest byte sizes match generated artifact content.
- [x] Add validation that manifest entries cover every generated and curated artifact except the manifest itself.
- [x] Add validation that the versioned `llms.txt` header carries no generation timestamp (so it stays semantic and listable in the manifest).
- [x] Add validation that every artifact listed in `llms.txt` exists (the linked corpora/catalog/manifest/guide/examples are all manifest-covered and existence-checked).
- [x] Add validation that Markdown files contain no unresolved `.. include::` directives.
- [x] Add validation that Markdown files contain no unresolved Sphinx roles such as `:ref:`.
- [x] Add validation that generated Markdown contains no accidental Sphinx-only substitution syntax.
- [x] Add a determinism check: rerun the generator at a pinned `SOURCE_DATE_EPOCH`, commit, and `--url-base`, and assert no file diffs (including the manifest).
- [x] Add a check that the sample JSONL records validate against the published JSON Schema.
- [x] Add a check that the sample records match the generator's output for those procedures (the samples are generator-derived, so they must be regenerated when the generator changes).
- [x] Compile curated/reused examples with wrapper compilers where practical (verified in Phase 4 with `mpicc -Wall`; the canonical `examples/` programs compile via `examples/Makefile`).
- [~] Run small example smoke tests where practical and reliable in CI. *(Examples are compile-checked; runtime smoke tests are left to the existing `examples/` CI.)*
- [~] Add Python compatibility checks for the new generator code against the 3.6 floor. *(All new Python uses 3.6-safe constructs — `str.format`, stdlib APIs available in 3.6; no automated 3.6-interpreter CI gate added.)*
- [x] Add docs-build coverage for the Sphinx-enabled path (exercised by `make`/`make check` here and by Read the Docs with `fail_on_warning`).
- [~] Add docs-build coverage, or a documented manual test, for the no-Sphinx pre-rendered tarball path. *(Documented; relies on the existing pre-rendered-tarball install path that the LLM artifacts ride.)*

## Phase 8: release and documentation process

Policy decisions for this phase (see spec "Release and maintenance process"):
schema is additive within a version with a one-release deprecation window;
version bumps only on breaking changes, owned by docs maintainers; schema
bumps and notable field changes get a changelog entry while routine regen
does not; artifacts get a one-time feature release note; release-time
regeneration relies on the RM's existing `SOURCE_DATE_EPOCH` practice;
publication is verified by a documented manual spot-check.

All process documentation lives in the new
`docs/developers/llm-friendly-docs.rst` page (linked from the developer-docs
index), with the contributor sync expectation also in `AGENTS.md`.

- [x] Document the schema compatibility policy: additive-only within a `schema_version`; a field may be deprecated (still emitted) for one release series before removal under a bump; no removal/rename/repurpose without a bump.
- [x] Document that `schema_version`/`artifact_schema_version` bump only on a breaking change, owned by docs maintainers.
- [x] Document that schema bumps and notable field changes get a `docs/release-notes/changelog/vMAJOR.MINOR.x.rst` entry, and that routine regeneration does not.
- [x] Add the one-time feature release note when the artifacts first ship (`docs/release-notes/changelog/v6.1.x.rst`, added to the changelog index).
- [x] Document the curated-doc sync expectation in **both** the developer docs and `AGENTS.md`: a PR changing public MPI docs should update affected curated LLM docs when relevant.
- [~] Add a lightweight CI drift hint (not a hard gate) for likely curated-doc gaps. *(The sync expectation is documented in the developer docs and AGENTS.md. Not implemented: the validator checks that the manifest inventories every on-disk artifact, but does **not** check procedure-to-record or curated-symbol coverage, so coverage gaps are currently silent. An automated drift hint is left as a follow-up — curated-doc staleness cannot be detected reliably.)*
- [x] Document that release-time regeneration relies on the RM's existing `SOURCE_DATE_EPOCH` reproducible-build practice; no separate regeneration step (in the developer LLM-docs page).
- [x] Document the manual post-merge publication spot-check: fetch `/llms.txt` and one versioned artifact (e.g., the manifest) and confirm they resolve and parse.

## Phase 9: follow-on work

- [x] Generate a Markdown corpus for the section-1 command man pages (`mpirun`, `ompi_info`, the wrapper compilers, ...) under `llms/man-openmpi/man1/`, indexed in `llms.txt`. *(Markdown only — commands are not MPI APIs, so no JSONL catalog records. Reuses the existing RST→Markdown converter; stays in sync because it regenerates from the RST on every build.)*
- [x] Use relative links for local (git) builds and absolute slug-correct links for Read the Docs builds. *(A local build targets a local LLM reading the files off disk, so every link — `llms.txt`, JSONL record `urls`, manifest `url`s, per-symbol/man1 Canonical-HTML headers — is relative to its own file; an RTD build uses `…/en/<slug>/…`. Implemented via the `LinkMaker` class.)*
- [x] Make `llms.txt` self-describing for an LLM audience. *(An "about this file" section names the build — for a `main` RTD build, both the `main` slug and the `vA.B.x` series — documents the `…/en/VERSION_SLUG/` scheme for finding other versions' LLM docs, and points to the legacy README/FAQ/doc pages for pre-v5.0.0.)*
- [~] Extend the same artifact model to OpenSHMEM public APIs. *(Deferred until someone asks for it, per the spec; not implemented.)*
- [~] Add OpenSHMEM-specific schema examples before generating the OpenSHMEM catalog. *(Deferred with the OpenSHMEM work above.)*

Considered and dropped: an `llms-full.txt` full-payload entry point, compressed
downloadable bundles, and stable redirects for renamed artifacts. JSON Schema
files were promoted into current scope (see Phases 1, 3, 5, and 7).
