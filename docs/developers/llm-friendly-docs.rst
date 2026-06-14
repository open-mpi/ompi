LLM-friendly documentation artifacts
====================================

In addition to the human-facing HTML and Unix man pages, the Open MPI
documentation build publishes machine-readable, LLM-friendly artifacts for
the public MPI APIs (C, ``mpif.h``, ``use mpi``, and ``use mpi_f08``). They
are indexed from a top-level ``llms.txt`` and published under ``llms/`` in
each documentation version (for example,
``https://docs.open-mpi.org/en/<version>/llms.txt``, where ``<version>`` is
the Read the Docs version slug, such as ``v6.1.x`` or ``main``).

The intent is to give LLMs, retrieval systems, and coding assistants concise,
authoritative, version-correct MPI API information --- a signature, its
parameters, the right language interface, and a link back to the human docs ---
*without* scraping themed HTML or guessing from filenames, and *without*
creating a second hand-maintained API reference that could drift from the real
documentation.

This page is for Open MPI developers and release managers who maintain these
artifacts. It records the design intent and the day-to-day maintenance rules;
the full design record (every field, every alternative considered) lives in
``specs/llms-friendly-docs/spec.md`` in the source tree.

Background: the llms.txt convention
-----------------------------------

``llms.txt`` is a convention proposed by Jeremy Howard in September 2024
(`llmstxt.org <https://llmstxt.org/>`_) for exposing LLM-friendly content at a
well-known location --- a Markdown file at the site root (``/llms.txt``),
alongside the established ``robots.txt`` and ``sitemap.xml``. The bare
convention is an *index*: an H1 project name, a short summary, and
H2-delimited lists of Markdown links, optionally with companion ``.md``
versions of HTML pages.

`Read the Docs supports this convention
<https://docs.readthedocs.com/platform/stable/reference/llms-txt.html>`_: it
does **not** auto-generate the file, but if a built, public, active default
version contains an ``llms.txt`` in its HTML output, Read the Docs serves it at
the project root.

Open MPI **extends** the convention rather than merely conforming to it. A bare
link index is not enough for a tool that needs to answer signature, parameter,
and interface questions programmatically, so alongside the ``llms.txt`` index
Open MPI also publishes a structured JSONL API catalog, language-specific
Markdown corpora, per-symbol Markdown pages, and a machine-readable manifest
(see `What is generated`_). The ``llms.txt`` index itself follows the spirit of
the convention (an H1 name, a summary, and link sections) but adds prose that an
automated consumer needs --- most importantly, a description of the URL scheme
for *other* versions (see `Read the Docs and the version-neutral llms.txt`_).

Design principles
-----------------

Five cross-cutting principles explain most of the decisions below. When in
doubt, preserve these properties.

* **The existing RST man pages and the MPI Forum JSON binding metadata are the
  only source of truth.** A core goal is to *avoid a second hand-maintained
  API reference*. The generated artifacts are derived, never authored: the set
  of documented procedures comes from the ``man-openmpi/man3/MPI_*.3.rst`` man
  pages and the command corpus from the ``man-openmpi/man1/*.rst`` pages, while
  standard-API signatures/parameters come from the MPI Forum metadata
  (``mpi-standard-apis.json`` via the embedded ``pympistandard`` library). This
  is why adding a new API is a matter of adding a man page, not editing the
  generator (see `Updating the documentation when APIs change`_), and why the
  curated examples reuse the top-level ``examples/`` tree instead of copying it.

* **Each artifact's content hash is a pure function of its semantic content.**
  Build-identity fields (``git_commit``, ``git_describe``, ``generated_at``)
  live **only** in the manifest --- never in the catalog records, corpora, or
  per-symbol pages. If a commit hash were embedded in every record, every
  artifact's hash would change on every repository commit, even one that touches
  no documentation, making the manifest's per-artifact hashes useless as change
  detectors. Keeping build identity in one place gives consumers the clean
  property *"hash changed* ⇔ *documentation content changed."* This is also why
  the versioned ``llms.txt`` is timestamp-free (it carries the version, not a
  generation time) and why the manifest is the one artifact that does not
  inventory itself.

* **Reproducible under Open MPI's existing knob.** The build fits the project's
  established reproducible-builds model rather than inventing a new one: the
  semantic artifacts contain no wall-clock data and are byte-identical across
  reruns at a given commit; only the manifest's ``generated_at`` varies, and it
  is derived from ``SOURCE_DATE_EPOCH`` when set (see `Reproducibility and the
  per-release manifest`_).

* **The schemas are simultaneously the published contract and the CI
  validator.** ``docs/llms-src/*.schema.json`` are shipped as artifacts *and*
  used directly by ``make check``, so the contract and the check cannot drift.
  They are "open" objects (no ``additionalProperties: false``) so that an
  additive field never breaks a consumer validating against an older cached
  copy of the schema (see `Schema evolution`_).

* **The artifacts ride inside the HTML output tree.** Rather than a parallel
  distribution mechanism, the ``llms/`` tree and ``llms.txt`` are copied into
  the Sphinx HTML output, so they inherit the existing HTML packaging,
  installation, and tarball machinery for free, and the link strategy follows
  the build type (see `How they are built`_ and `Link strategy: relative vs.
  absolute links`_).

What is generated
-----------------

* ``llms/openmpi-mpi-api.jsonl`` --- one JSON record per documented MPI
  procedure (standard, extension, and deprecated/removed). A record is
  per *procedure*, not per page.
* ``llms/openmpi-mpi-api.md`` and the four per-interface corpora (C, ``mpif.h``,
  ``use mpi``, ``use mpi_f08``). ``mpif.h`` and ``use mpi`` share the same
  ``f90`` signature and differ only in the access preamble, so those two
  corpora are near-duplicates by design; they are kept separate for audience
  clarity and possible future divergence.
* ``llms/man-openmpi/man3/MPI_<name>.3.md`` --- one Markdown page per man page,
  1:1 with the human man pages so canonical URLs line up. Overview/non-procedure
  pages (e.g. ``MPI_T.3``, ``MPI_Errors.3``) get a Markdown page but no JSONL
  record.
* ``llms/man-openmpi/man1/<command>.1.md`` --- one Markdown page per command man
  page (``mpirun``, ``ompi_info``, the wrapper compilers, ...). These document
  Open MPI *commands*, not MPI APIs, so they are a Markdown corpus only ---
  there are no JSONL catalog records for them.
* ``llms/openmpi-docs-manifest.json`` --- the artifact inventory; the only
  artifact that carries build identity (git commit/describe and
  ``generated_at``).
* The curated ``llms/openmpi-mpi-interface-guide.md`` and
  ``llms/openmpi-mpi-examples.md`` (hand-written sources under
  ``docs/llms-src/``), plus the two published ``*.schema.json`` files.

How they are built
------------------

``docs/generate-llm-docs.py`` produces the artifacts into a build-tree staging
directory (``docs/llms-build/``). It is run in both documentation build paths,
just like the man-page bindings generator: from a sentinel target in
``docs/Makefile.am`` (after the man3 bindings, before Sphinx) for ``make``
builds, and from ``.readthedocs-pre-create-environment.sh`` for Read the Docs
builds (which run ``sphinx-build`` directly and never run ``make``). Shared MPI
metadata logic (binding rendering, ``VERSION`` parsing, build identity) lives in
``docs/ompi_docs_common.py``, which is also used by the man-page bindings
generator. No separate command is needed: building the docs builds the LLM
artifacts.

A ``build-finished`` hook in ``docs/conf.py`` then copies the staging tree
(``llms-build/llms`` → ``<output>/llms`` and ``llms-build/llms.txt`` →
``<output>/llms.txt``) into the Sphinx HTML output. The copy lives in a Sphinx
hook --- rather than ``html_extra_path`` or a Makefile step --- for two
reasons: (1) the staged Markdown is deliberately excluded from Sphinx source
discovery (``exclude_patterns``), which would *also* suppress an
``html_extra_path`` entry pointing at it; and (2) the hook runs inside
``sphinx-build``, so publication works identically under ``make`` and on Read
the Docs. Because the artifacts ride inside the HTML output tree, the existing
``html-local`` / ``EXTRA_DIST`` (tarballs), ``install-data-hook`` (install), and
``uninstall-hook`` (uninstall) machinery handles them with no new rules.

Link strategy: relative vs. absolute links
-------------------------------------------

Every generated link --- in ``llms.txt``, in the JSONL records' ``urls``, in the
manifest ``url`` fields, and in the per-symbol/man1 page Canonical-HTML headers
--- follows the build type:

* A **local build from git, or a release tarball,** produces the artifacts for a
  *local* tool that reads the files straight off disk. There is no
  ``docs.open-mpi.org`` site in play, so every link is made **relative to the
  file that contains it**. The result is a self-contained, portable tree that
  resolves no matter where it lives (or is unpacked from a tarball), and which
  needs no network access to follow internal links.
* A **Read the Docs build** publishes under ``https://docs.open-mpi.org/en/<slug>/``,
  so every link is **absolute** and uses that published version slug
  (``.../en/main/...`` for the ``main`` branch, ``.../en/v6.1.x/...`` for a
  release branch, ``.../en/v6.1.0/...`` for a tagged release). Absolute links
  mean a record copied *out* of the published site --- into a vector store, a
  prompt, a cache --- still resolves back to the correct version's
  documentation.

The base URL is resolved in this order: an explicit ``--url-base`` /
``OMPI_LLM_URL_BASE`` override; then ``READTHEDOCS_CANONICAL_URL`` /
``READTHEDOCS_VERSION`` (set by Read the Docs); otherwise relative. This logic
lives in the ``LinkMaker`` class in ``docs/generate-llm-docs.py``. The one
exception: the version-slug *scheme-documentation* URLs printed inside
``llms.txt`` (the ``.../en/VERSION_SLUG/`` examples) are always literal absolute
``docs.open-mpi.org`` text, even in a local build, so a local consumer still
learns where the published versions live.

Because both passes (local and Read the Docs) each pick one strategy and hold it
constant, the determinism check (which generates twice and diffs) is unaffected.

Read the Docs and the version-neutral llms.txt
----------------------------------------------

Read the Docs serves the version-neutral ``https://docs.open-mpi.org/llms.txt``
by serving **the default version's own** ``llms.txt`` at the site root. There is
no separate, hand-maintained top-level index file: the root URL is simply
whichever ``llms.txt`` the current default version produced. Each documentation
version emits exactly one self-describing ``llms.txt`` for itself.

This RTD behavior directly drove the *content* of ``llms.txt``. Because the file
served at the root is just some version's file --- and the default version may
be a series that does not even carry these artifacts yet --- ``llms.txt`` cannot
assume it is authoritative for the whole project. So it **self-describes the
version-slug URL scheme**:

* it states the ``https://docs.open-mpi.org/en/VERSION_SLUG/`` scheme and how to
  read a slug (``main`` = the main-branch build; ``vA.B.x`` = a release branch;
  ``vA.B.C`` = a specific tagged release), worded so an LLM that wants a
  *different* version can construct the URL itself;
* it notes that documentation for Open MPI versions older than v5.0.0 is not
  published in this format, and points at the legacy README/FAQ/doc pages;
* for a Read the Docs ``main`` build it uses **dual attribution** --- naming both
  the ``main`` slug and the ``vA.B.x`` series that build currently represents ---
  because the same file is both "the development tip" and "the current
  pre-release series."

If a future need arises for the root ``/llms.txt`` to be something other than the
default version's copy, Read the Docs exact redirects can point ``/llms.txt`` at
a chosen versioned path; no generator change would be required.

Validation
----------

``make check`` builds the artifacts if they are not already present and then
runs ``docs/validate-llm-docs.py``, which validates the catalog and manifest
against the published JSON Schemas (``docs/llms-src/*.schema.json``), checks
cross-field invariants (for example, that a record's ``languages`` equals the
distinct set of its ``bindings[].language``) and manifest integrity (hashes,
byte sizes, coverage), confirms the generated Markdown is free of unresolved RST
(``.. include::`` directives, ``:ref:`` roles, Sphinx-only substitutions),
confirms the versioned ``llms.txt`` carries no generation timestamp, verifies the
committed sample records (``specs/llms-friendly-docs/sample-records.jsonl``)
match the generated catalog, and confirms the generator is deterministic at a
fixed ``SOURCE_DATE_EPOCH``.

Reproducibility and the per-release manifest
--------------------------------------------

The artifacts are regenerated by the normal documentation build, so there is no
separate regeneration step for release managers, and no separate command for
developers: ``make`` (or a Read the Docs build) regenerates everything,
including the manifest, every time.

The manifest (``llms/openmpi-docs-manifest.json``) is rebuilt on every build and
is the single place that carries build identity: ``git_commit``,
``git_describe``, ``generated_at``, the Open MPI version/series (from the
top-level ``VERSION`` file), the Read the Docs slug when present, and one entry
per artifact (path, URL, media type, SHA-256, byte size, estimated token count,
and the symbols/languages it covers). It does **not** inventory itself --- a file
cannot record its own hash and size without changing them. Consumers do not need
a separately incremented "docs release number": they compare the manifest's
identity fields and per-artifact hashes. If, say, the ``v6.1.x`` branch gets a
documentation fix before ``v6.1.1`` ships, the Open MPI version may be unchanged
but the git identity and the hashes of the *affected* artifacts change, while
everything else stays byte-identical.

For a reproducible release tarball, set ``SOURCE_DATE_EPOCH`` (as already
documented for reproducible Open MPI builds): the generator honors it for the
manifest ``generated_at`` timestamp, exactly as ``config/getdate.sh`` and
Sphinx's ``format_date`` do for the rest of the docs build. With
``SOURCE_DATE_EPOCH`` set, the whole documentation build (HTML, man pages, and
LLM artifacts) is reproducible. In a from-tarball build with no ``.git`` and no
``SOURCE_DATE_EPOCH``, the generator degrades gracefully rather than failing:
the git fields become ``unknown``/omitted and ``generated_at`` falls back to the
build date, mirroring ``config/opal_get_version.sh``. Distribution tarballs ship
the already-rendered ``llms/`` tree inside ``html/``, so installing or packaging
them never requires Sphinx; Sphinx is needed only to *regenerate* them in a
developer clone.

Schema evolution
----------------

The catalog and manifest each conform to a published JSON Schema that doubles as
the CI validator.

* Changes are **additive within a** ``schema_version``: new fields must be
  optional, and a field may be marked deprecated (still emitted) for one Open
  MPI release series before removal. Never remove, rename, repurpose, or change
  the meaning of an existing field without bumping the schema version. The
  published schemas use open objects so an additive field does not break a
  consumer validating against an older cached schema.
* ``schema_version`` / ``artifact_schema_version`` are bumped **only on a
  breaking change**, a decision owned by the documentation maintainers. The
  ``schema_version`` is independent of the Open MPI release number, so the
  schema can stay fixed across many releases.
* A schema-version bump or notable field change gets a changelog entry under
  ``docs/release-notes/changelog/``. Routine regeneration (content that flows
  automatically from changed RST or metadata) does not.

Updating the documentation when APIs change
-------------------------------------------

The generated artifacts cannot drift, because they regenerate from the RST man
pages and the MPI Forum JSON binding metadata. The practical consequence is that
**you update
the LLM docs by updating the ordinary documentation**, not by editing the
generator.

**Adding or changing an MPI API function.** When a new MPI Standard function is
added (for example, when a new MPI version lands), the LLM artifacts pick it up
automatically *once its man page exists*:

#. Add or edit ``docs/man-openmpi/man3/MPI_<Name>.3.rst`` --- the same
   hand-written man page that produces the human HTML/man output. This man page
   is what makes the function "documented"; both the man3 binding generator and
   the LLM generator enumerate the man3 ``.rst`` files (via ``os.listdir``), so
   a function that is in the metadata but has no ``.rst`` page is silently *not*
   documented anywhere.
#. Add the page to the explicit ``OMPI_MAN3`` list in ``docs/Makefile.am``. The
   generators auto-discover the file and the ``RST_SOURCE_FILES`` wildcard
   already makes it a rebuild dependency, but the *installation* list is
   explicit (Automake installs man pages by name), so a new page must be listed
   there to be installed.
#. For standard APIs there is **nothing to author for the bindings**: the C,
   ``mpif.h``/``use mpi``, and ``use mpi_f08`` signatures (and any large-count
   "embiggened" variant) are rendered from ``pympistandard`` +
   ``mpi-standard-apis.json``.
#. If one man page documents several procedures, mark them with a
   ``.. mpi-bindings: MPI_Foo, MPI_Bar`` comment line so each co-documented
   procedure gets its own bindings, catalog record, and ``documented_with``
   linkage.

No change to ``generate-llm-docs.py``, the schemas, or the validator is needed
for a routine new function: a new ``.rst`` automatically yields a new man page,
a per-symbol Markdown page, and a JSONL catalog record.

**Open MPI extensions** (``MPIX_*``, ``OMPI_*``) are not in ``pympistandard``, so
their signatures are taken **verbatim** from the RST ``SYNTAX`` block and their
structured parameter fields are best-effort (``unknown`` where they cannot be
extracted reliably). The catalog's ``kind`` field lets consumers tell standard
records from extension records.

**Upgrading the MPI Standard metadata** (for example, replacing the 4.1
``apis.json`` with a 5.0 one): update ``docs/mpi-standard-apis.json`` (it is a
symlink to the versioned ``mpi-standard-<ver>-apis.json``). Note that
``load_pympistandard`` calls ``use_api_version(1, ...)`` --- the ``1`` is
``pympistandard``'s *data-format* version, **not** the MPI version. A new MPI
metadata file is only loadable if the vendored ``3rd-party/pympistandard`` can
parse that format; if the MPI Forum bumped the JSON format, update the vendored
library in lockstep and smoke-test that the new JSON loads.

**Coverage gaps are currently silent.** There is no check that flags a procedure
present in the metadata but missing a man page (or vice versa); such a procedure
is simply absent from the artifacts. (The spec envisions a lightweight CI
"drift hint" for this; it is not yet implemented.) When adding a batch of new
functions, cross-check that every intended function actually has an
``MPI_*.3.rst`` page.

**Curated docs and samples.** The generated artifacts cannot drift, but the
curated ``docs/llms-src/`` files (interface guide and examples) can. A pull
request that changes public MPI documentation should also update the affected
curated files when relevant (this expectation is also recorded in the top-level
``AGENTS.md``). When the curated examples or a schema change, regenerate
``specs/llms-friendly-docs/sample-records.jsonl`` so ``make check`` continues to
pass.

Verifying publication
---------------------

After a merge that affects the artifacts, confirm Read the Docs published them
for the version you merged to: fetch that version's ``llms.txt`` (for example
``https://docs.open-mpi.org/en/main/llms.txt`` for the ``main`` branch) and one
versioned artifact (for example the manifest,
``.../en/<version>/llms/openmpi-docs-manifest.json``) and confirm they resolve
and parse. Because the version-neutral ``https://docs.open-mpi.org/llms.txt``
follows Read the Docs' *default* version --- which may not be the version you
just merged --- always spot-check the specific version slug you published.

Alternatives considered and deferred
------------------------------------

For the historical record (details in ``specs/llms-friendly-docs/spec.md``):

* **OpenSHMEM** public APIs can later reuse this same artifact model, but support
  is **deferred until there is concrete demand**; this effort covers MPI only.
* An ``llms-full.txt`` single-payload entry point, compressed downloadable
  bundles, and stable redirects for renamed artifacts were each considered and
  **dropped** --- the aggregate Markdown corpora, the release tarballs plus the
  manifest, and ad hoc Read the Docs redirects respectively make them
  unnecessary. The JSON Schema files, by contrast, were promoted *into* scope as
  the shared contract/validator.
