# Discovering Open MPI releases

The rest of this corpus describes one version of Open MPI. To learn which
Open MPI versions exist — for example, to check whether a newer release is
available, or to find the download URL and checksum of a release — use the
machine-readable files published on the Open MPI web site. They are generated
from the same data as the human-facing download pages, so they always agree
with them. Do not scrape the download pages, and do not use Git tags (a
release is tagged before it is published) or GitHub-generated tarballs (they
are not official releases and do not build).

The human-readable version of this guide is the "Detecting new releases
programmatically" section of the "Downloading Open MPI" page in the Open MPI
documentation.

These files cover Open MPI v4.1.0 and later. Older releases are ancient: they
are not listed (they remain downloadable from the web site's download pages).

Prefer the JSON documents: they have the most information (every version,
release dates, file URLs, sizes, and checksums). The plain-text files are a
shortcut for "what is the latest version?", and the Atom feeds exist for
people's feed readers; they carry less information.

## Quick answers

- Latest recommended release (plain text, e.g. `5.0.11`):
  `https://www.open-mpi.org/software/ompi/current/downloads/latest_release.txt`
- Latest release in series `vA.B` (plain text; HTTP 404 if the series has no
  releases yet):
  `https://www.open-mpi.org/software/ompi/vA.B/downloads/latest_release.txt`
- Every series and every version (JSON):
  `https://www.open-mpi.org/software/ompi/releases.json`
- Release dates, file URLs, sizes, and checksums for series `vA.B` (JSON):
  `https://www.open-mpi.org/software/ompi/vA.B/downloads/releases.json`

The plain-text files contain only a version string with no trailing newline
and carry a `Last-Modified` header set to that release's time;
strip whitespace before comparing.
`https://www.open-mpi.org/software/ompi/vA.B/downloads/latest_snapshot.txt`
also exists: it reports the series' latest *prerelease* (e.g. `6.0.0rc2`) if
there is one, otherwise its latest release. Despite the name, it is not about
nightly snapshot tarballs.

## `releases.json` (index)

```json
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
    }
  ]
}
```

- `current_series` is the series the Open MPI community recommends;
  `latest_release` is that series' newest release. Newer series may exist with
  only prereleases.
- `series` lists every release series, newest first. `releases` and
  `prereleases` list every version in the series, newest first;
  `latest_release` / `latest_prerelease` are their first elements, or `null`.
  Prereleases are typically removed from `prereleases` once the corresponding
  final release is out.

## `vA.B/downloads/releases.json` (per series)

The same fields as the series' entry in the index, plus `release_details`:
one object for every version in `prereleases` and then every version in
`releases` (each list newest first).

```json
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
```

`sha256`, `sha1`, and `md5` checksums are provided; verify downloads with
`sha256`.

## Atom feeds (secondary)

For people's feed readers and chat/email integrations. Programs, including
agents, should use the JSON documents instead: the feeds carry less
information (for example, no checksums).

- All series, the 25 most recent releases:
  `https://www.open-mpi.org/software/ompi/releases.atom`
- One series, every release:
  `https://www.open-mpi.org/software/ompi/vA.B/downloads/releases.atom`

Both feeds include prereleases. Each entry is one release (or prerelease): a permanent `<id>`
(`tag:open-mpi.org,2026:openmpi/release/VERSION`), title `Open MPI VERSION`
(plus ` (prerelease)`), publication time, a link to the series download page,
one `rel="enclosure"` link per downloadable file (URL and size; no checksums),
and categories `release`/`prerelease` and the series (e.g. `v5.0`).

## Rules for consumers

- A version that appears in these files is fully published (downloadable, web
  page live).
- Compare versions numerically, not lexically: `5.0.10` is newer than `5.0.9`.
- The JSON documents and Atom feeds send an `ETag` and honor `If-None-Match`
  (HTTP 304 when unchanged). Poll no more often than every few hours.
- The per-series JSON documents and the Atom feeds return HTTP 503 with
  `Retry-After` instead of an incomplete document when release details are
  temporarily unavailable.
- `schema_version` changes only for incompatible changes. Fields may be added
  without changing it; ignore unknown fields.
