.. _label-version-numbers:

Version Numbers and Binary Compatibility
========================================

OpenPMIx has two sets of version numbers that are likely of interest
to end users / system administrator:

* Software version number
* Shared library version numbers

Both are described below, followed by a discussion of application
binary interface (ABI) compatibility implications.

Software Version Number
-----------------------

OpenPMIx's version numbers are the union of several different values:
major, minor, release, and an optional quantifier.

* Major: The major number is the first integer in the version string
  (e.g., v1.2.3). Changes in the major number typically indicate a
  significant change in the code base and/or end-user
  functionality. The major number is always included in the version
  number.

* Minor: The minor number is the second integer in the version
  string (e.g., v1.2.3). Changes in the minor number typically
  indicate a incremental change in the code base and/or end-user
  functionality. The minor number is always included in the version
  number:

* Release: The release number is the third integer in the version
  string (e.g., v1.2.3). Changes in the release number typically
  indicate a bug fix in the code base and/or end-user
  functionality.

* Quantifier: OpenPMIx version numbers sometimes have an arbitrary
  string affixed to the end of the version number. Common strings
  include:

  * ``aX``: Indicates an alpha release. X is an integer indicating
    the number of the alpha release (e.g., v1.2.3a5 indicates the
    5th alpha release of version 1.2.3).
  * ``bX``: Indicates a beta release. X is an integer indicating
    the number of the beta release (e.g., v1.2.3b3 indicates the 3rd
    beta release of version 1.2.3).
  * ``rcX``: Indicates a release candidate. X is an integer
    indicating the number of the release candidate (e.g., v1.2.3rc4
    indicates the 4th release candidate of version 1.2.3).

Although the major, minor, and release values (and optional
quantifiers) are reported in OpenPMIx nightly snapshot tarballs, the
filenames of these snapshot tarballs follow a slightly different
convention.

Specifically, the snapshot tarball filename contains three distinct
values:

* Most recent Git tag name on the branch from which the tarball was
  created.

* An integer indicating how many Git commits have occurred since
  that Git tag.

* The Git hash of the tip of the branch.

For example, a snapshot tarball filename of
``pmix-v1.0.2-57-gb9f1fd9.tar.bz2`` indicates that this tarball was
created from the v1.0 branch, 57 Git commits after the ``v1.0.2`` tag,
specifically at Git hash gb9f1fd9.

OpenPMIx's Git master branch contains a single ``dev`` tag.  For example,
``pmix-dev-8-gf21c349.tar.bz2`` represents a snapshot tarball created
from the master branch, 8 Git commits after the "dev" tag,
specifically at Git hash gf21c349.

The exact value of the "number of Git commits past a tag" integer is
fairly meaningless; its sole purpose is to provide an easy,
human-recognizable ordering for snapshot tarballs.

Shared Library Version Number
-----------------------------

OpenPMIx uses the GNU Libtool shared library versioning scheme.

.. note:: Only official releases of OpenPMIx adhere to this versioning
          scheme. "Beta" releases, release candidates, and nightly
          tarballs, developer snapshots, and Git snapshot tarballs
          likely will all have arbitrary/meaningless shared library
          version numbers.

The GNU Libtool official documentation details how the versioning
scheme works.  The quick version is that the shared library versions
are a triple of integers: (current,revision,age), or ``c:r:a``.  This
triple is not related to the PMIx software version number.  There
are six simple rules for updating the values (taken almost verbatim
from the Libtool docs):

#. Start with version information of ``0:0:0`` for each shared library.

#. Update the version information only immediately before a public
   release of your software. More frequent updates are unnecessary,
   and only guarantee that the current interface number gets larger
   faster.

#. If the library source code has changed at all since the last
   update, then increment revision (``c:r:a`` becomes ``c:r+1:a``).

#. If any interfaces have been added, removed, or changed since the
   last update, increment current, and set revision to 0.

#. If any interfaces have been added since the last public release,
   then increment age.

#. If any interfaces have been removed since the last public release,
   then set age to 0.

Application Binary Interface (ABI) Compatibility
------------------------------------------------

OpenPMIx provides forward ABI compatibility in all versions of a given
feature release series and its corresponding
super stable series.  For example, on a single platform, a PMIx
application linked against OpenPMIx v1.3.2 shared libraries can be
updated to point to the shared libraries in any successive v1.3.x or
v1.4 release and still work properly (e.g., via the ``LD_LIBRARY_PATH``
environment variable or other operating system mechanism).

OpenPMIx reserves the right to break ABI compatibility at new feature
release series.  For example, the same PMIx application from above
(linked against PMIx v1.3.2 shared libraries) will *not* work with
PMIx v1.5 shared libraries.
