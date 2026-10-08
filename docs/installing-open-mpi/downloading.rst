
.. _building-open-mpi-downloading-label:

Downloading Open MPI
====================

Open MPI is generally available two ways:

#. As source code.

   * The best place to get an official Open MPI source code
     distribution is from `the main Open MPI web site
     <https://www.open-mpi.org/>`_.

   * Downstream Open MPI packagers (e.g., Linux distributions)
     sometimes also provide source code distributions.  They may
     include additional patches or modifications.

     Consult your favorite downstream packager for more details.
     
   .. caution:: Do **not** download an Open MPI source code tarball
               from GitHub.com.  The tarballs automatically generated
               by GitHub.com are incomplete and will not build
               properly.

               GitHub.com-generated tarballs are **not** official Open
               MPI releases.

#. As binary packages.

   * The Open MPI community does not provide binary packages on `the
     main Open MPI web site <https://www.open-mpi.org/>`_.

   * Various downstream packagers (e.g., Linux distributions,
     Homebrew, MacPorts, etc.) *do* provide pre-built, binary packages
     for Open MPI.

     Consult your favorite downstream packager for more details.

Most of the remaining pages of this part of the documentation deal
with installing Open MPI from source; the next section covers
detecting new releases programmatically.

.. _label-install-detecting-new-releases:

Detecting new releases programmatically
---------------------------------------

Packagers, CI systems, and automated agents can discover new Open MPI
releases without scraping the download web pages: the Open MPI web
site publishes machine-readable files that are generated from the same
data as the download pages, so they always agree with them.

* **The JSON documents are the recommended interface.**  They carry
  the most information: every release and prerelease in every series,
  release dates, and each release's downloadable files with URLs,
  sizes, and checksums.
* The plain-text files are a convenience for simple checks (e.g., in
  a shell script) that only need the latest version number.
* The Atom feeds are a convenience for subscribing to release
  announcements in feed readers or chat/email integrations.  They
  carry less information than the JSON documents (e.g., no
  checksums).

These files cover Open MPI v4.1.0 and later (i.e., the v4.1 and later
release series).  Releases older than that are ancient: they are still
available from the Open MPI web site's download pages, but they are not
listed in these files, and their release series do not have
per-series files.

JSON documents
^^^^^^^^^^^^^^

Two JSON documents are available:

* https://www.open-mpi.org/software/ompi/releases.json: an index
  of every Open MPI release series and every version released in
  each.  Use this to detect any new release in any series.

* ``https://www.open-mpi.org/software/ompi/vA.B/downloads/releases.json``
  (e.g., https://www.open-mpi.org/software/ompi/v5.0/downloads/releases.json):
  everything in the index's entry for the ``vA.B`` series, plus the
  release date and the downloadable files (with URLs, sizes, and
  checksums) of every release and prerelease in that series.

The index looks like this (abridged):

.. code-block:: json

   {
       "schema_version": 1,
       "project": "Open MPI",
       "current_series": "5.0",
       "latest_release": "5.0.11",
       "series": [
           {
               "series": "6.0",
               "branch": "v6.0.x",
               "latest_release": null,
               "latest_prerelease": "6.0.0rc2",
               "releases": [],
               "prereleases": ["6.0.0rc2", "6.0.0rc1"],
               "web_page_url": "https://www.open-mpi.org/software/ompi/v6.0/",
               "download_url_prefix": "https://download.open-mpi.org/release/open-mpi/v6.0/",
               "details_url": "https://www.open-mpi.org/software/ompi/v6.0/downloads/releases.json"
           },
           {
               "series": "5.0",
               "branch": "v5.0.x",
               "latest_release": "5.0.11",
               "latest_prerelease": null,
               "releases": ["5.0.11", "5.0.10", "...", "5.0.0"],
               "prereleases": [],
               "...": "..."
           }
       ]
   }

Top-level fields of the index:

* ``schema_version``: the version of this format; see below.
* ``project``: always ``Open MPI``.
* ``current_series``: the release series that the Open MPI community
  currently recommends (e.g., ``5.0``).  Note that newer series may
  exist (e.g., a series that only has prereleases so far).
* ``latest_release``: the latest release of ``current_series``.
* ``series``: an array with one entry per release series, newest
  series first.

Each entry in ``series`` (and the top level of the per-series
document) contains:

* ``series``: the series name, ``A.B`` (no leading ``v``).
* ``branch``: the Open MPI Git branch the series is released from.
* ``latest_release`` / ``latest_prerelease``: the newest release /
  prerelease in the series, or ``null`` if there is none.
* ``releases`` / ``prereleases``: every release / prerelease version
  in the series, newest first.  Prereleases are typically removed
  from ``prereleases`` once the corresponding final release is out.
* ``web_page_url``: the human-readable download page for the series.
* ``download_url_prefix``: the URL prefix of the series' downloadable
  files.
* ``details_url``: the URL of the per-series JSON document.

The per-series document adds a ``release_details`` array, with one
entry for every version in ``prereleases`` and then ``releases``
(each newest first):

.. code-block:: json

   {
       "version": "5.0.11",
       "prerelease": false,
       "release_date": "2026-09-16T22:13:00Z",
       "release_unix_time": 1789596780,
       "files": [
           {
               "name": "openmpi-5.0.11.tar.bz2",
               "url": "https://download.open-mpi.org/release/open-mpi/v5.0/openmpi-5.0.11.tar.bz2",
               "size": 45250787,
               "sha256": "e668a3c4acd50c41dc204c8a6dd98a611e0f26af89cf677577fa9be8a2698003",
               "sha1": "02fa478148ebe764013bcc118930681952b89a31",
               "md5": "284035e88115c3da0f9812ce1141232f"
           }
       ]
   }

* ``release_date`` / ``release_unix_time``: when the release was
  published, as an ISO 8601 UTC timestamp and as seconds since the
  Unix epoch.
* ``files``: every downloadable file of the release (source tarballs,
  SRPMs, etc.), sorted by name.  ``size`` is in bytes.  ``sha256``,
  ``sha1``, and ``md5`` checksums are provided; verify downloads with
  ``sha256``.

Plain-text files
^^^^^^^^^^^^^^^^

For simple checks, each of these returns a single version string
(e.g., ``5.0.11``), with no trailing newline.  Strip any surrounding
whitespace before comparing.

.. list-table::
   :header-rows: 1
   :widths: 50 50

   * - URL
     - Contents
   * - https://www.open-mpi.org/software/ompi/current/downloads/latest_release.txt
     - The latest release of the current (i.e., recommended) release
       series.
   * - ``https://www.open-mpi.org/software/ompi/vA.B/downloads/latest_release.txt``
       (e.g., https://www.open-mpi.org/software/ompi/v5.0/downloads/latest_release.txt)
     - The latest release in the ``vA.B`` release series (e.g.,
       ``v5.0``).  Never reports a prerelease.  Returns HTTP 404 if
       the series has no releases yet.
   * - ``https://www.open-mpi.org/software/ompi/vA.B/downloads/latest_snapshot.txt``
       (e.g., https://www.open-mpi.org/software/ompi/v6.0/downloads/latest_snapshot.txt)
     - The latest *prerelease* in the ``vA.B`` series (e.g.,
       ``6.0.0rc2``) if the series currently has any prereleases;
       otherwise, the latest release in that series.

Each response carries a ``Last-Modified`` header set to the time of
the release it reports.

.. note:: Despite its name, ``latest_snapshot.txt`` does *not* report
          nightly snapshot tarballs; the name is historical.  Nightly
          snapshot tarballs have their own ``latest_snapshot.txt``
          files (e.g.,
          https://download.open-mpi.org/nightly/open-mpi/main/latest_snapshot.txt),
          which contain a snapshot version string as described in
          :ref:`the version numbering section
          <version_numbers_section_label>`.

Atom feeds
^^^^^^^^^^

For feed readers and chat or email integrations, `Atom
<https://www.rfc-editor.org/rfc/rfc4287>`_ feeds are also available.
Programs should use the JSON documents instead: the feeds are a
subset of the same information, in a format meant for people's feed
readers.

* https://www.open-mpi.org/software/ompi/releases.atom: the 25
  most recent releases across all release series.

* ``https://www.open-mpi.org/software/ompi/vA.B/downloads/releases.atom``
  (e.g., https://www.open-mpi.org/software/ompi/v5.0/downloads/releases.atom):
  every release in the ``vA.B`` series.

Both feeds include prereleases.  Each release series' download page (e.g.,
https://www.open-mpi.org/software/ompi/v5.0/) also advertises both
feeds for feed reader auto-discovery, so pasting that page's URL into
a feed reader works, too.

Each feed entry is one release (or prerelease):

* ``<id>``: a permanent identifier for the release, of the form
  ``tag:open-mpi.org,2026:openmpi/release/VERSION``.  It never changes.
* ``<title>``: ``Open MPI VERSION``, with ``(prerelease)`` appended
  for prereleases.
* ``<published>`` / ``<updated>``: when the release was published.
* ``<link rel="alternate">``: the release series' download page.
* ``<link rel="enclosure">``: one per downloadable file, with its
  URL, size (``length``), and file name (``title``).  Atom has no
  standard place for checksums; get them from the download page or
  the JSON documents.
* ``<category>``: ``release`` or ``prerelease``, and the series
  (e.g., ``v5.0``).

Using these files
^^^^^^^^^^^^^^^^^

* A version listed in these files is fully published: its files are
  downloadable and its web page is live.
* To detect a new release, compare ``latest_release`` (of the index,
  or of the series you track) with the last version you saw.  Do not
  compare version strings lexically (e.g., ``5.0.10`` is newer than
  ``5.0.9``); compare the numeric components, or simply check whether
  the version is in a list you have already seen.
* The JSON documents and Atom feeds carry an ``ETag`` header and
  honor ``If-None-Match``, so a poller can cheaply learn that nothing
  has changed (HTTP 304).  Please poll no more often than every few
  hours; releases happen no more than a few times a month.
* The per-series JSON documents and the Atom feeds return HTTP 503
  (with a ``Retry-After`` header) rather than an incomplete document
  if the release details are temporarily unavailable; retry later.
* ``schema_version`` changes only for incompatible changes.  New
  fields may be added at any time without changing it, so ignore
  fields you do not recognize.

.. caution:: Do not use Git tags in the `Open MPI GitHub repository
             <https://github.com/open-mpi/ompi>`_ to detect releases.
             A release is tagged before it is published, and, as
             noted above, GitHub.com-generated tarballs are not
             official Open MPI releases.
