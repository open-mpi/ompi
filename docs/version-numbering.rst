.. _version_numbers_section_label:

Version numbers and compatibility
=================================

Open MPI has two sets of version numbers that are likely of interest
to end users / system administrators:

#. Software version number
#. Shared library version numbers

Both are predicated on Open MPI's definition of "backward compatibility."

Backward Compatibility
----------------------

Open MPI version Y is backward compatible with Open MPI version X
(where Y>X) if users can:

* Compile an MPI/OpenSHMEM application with version X,
  ``mpirun``/``oshrun`` it with version Y, and get the same
  user-observable behavior.
* Invoke ``ompi_info`` with the same CLI options in versions X and Y and
  get the same user-observable behavior.

Note that this definition encompasses several things:

* Application Binary Interface (ABI)
* MPI / OpenSHMEM run time system
* ``mpirun`` / ``oshrun`` command line options
* MCA parameter names / values / meanings

However, this definition only applies when the same version of Open
MPI is used with all instances of the runtime and MPI / OpenSHMEM
processes in a single MPI job.  If the versions are not exactly the
same everywhere, Open MPI is not guaranteed to work properly in any
scenario.

Backward compatibility tends to work best when user applications are
dynamically linked to one version of the Open MPI / OSHMEM libraries,
and can be updated at run time to link to a new version of the Open
MPI / OSHMEM libraries.

For example, if an MPI / OSHMEM application links statically against
the libraries from Open MPI vX, then attempting to launch that
application with ``mpirun`` / ``oshrun`` from Open MPI vY is not guaranteed to
work (because it is mixing vX and vY of Open MPI in a single job).

Similarly, if using a container technology that internally bundles all
the libraries from Open MPI vX, attempting to launch that container
with ``mpirun`` / ``oshrun`` from Open MPI vY is not guaranteed to work.

Open MPI |ompi_series| MCA parameter compatibility
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Several MCA parameters have been deprecated in Open MPI |ompi_series|, please
see this :ref:`table <label-mca-backward-compat>` for the full list.

Software Version Number
-----------------------

Official Open MPI releases use the common "A.B.C" version identifier
format.  Each of the three numbers has a specific meaning:

* Major: The major number is the first integer in the version string
  Changes in the major number typically indicate a significant
  change in the code base and/or end-user functionality, and also
  indicate a break from backward compatibility.  Specifically: Open
  MPI releases with different major version numbers are not
  backward compatibale with each other.

  .. important:: This rule does not extend to versions prior to
     v1.10.0.  Specifically: v1.10.x is not guaranteed to be backward
     compatible with other v1.x releases.

* Minor: The minor number is the second integer in the version string.
  Changes in the minor number indicate a user-observable change in the
  code base and/or end-user functionality.  Backward compatibility
  will still be preserved with prior releases that have the same major
  version number (e.g., v2.5.3 is backward compatible with v2.3.1).

* Release: The release number is the third integer in the version
  string.  Changes in the release number typically indicate a bug fix
  in the code base and/or end-user functionality.  For example, if
  there is a release that only contains bug fixes and no other
  user-observable changes or new features, only the third integer will
  be increased (e.g., from v4.3.0 to v4.3.1).

The "A.B.C" version number may optionally be followed by a quantifier
string:

* ``aX``: Indicates an alpha release. X is an integer indicating the
  number of the alpha release (e.g., v1.10.3a5 indicates the 5th alpha
  release of version 1.10.3).
* ``bX``: Indicates a beta release. X is an integer indicating the
  number of the beta release (e.g., v1.10.3b3 indicates the 3rd beta
  release of version 1.10.3).
* ``rcX``: Indicates a release candidate. X is an integer indicating
  the number of the release candidate (e.g., v1.10.3rc4 indicates the
  4th release candidate of version 1.10.3).

Nightly development snapshot tarballs use a different version number
scheme; they contain three distinct values:

* The git branch name from which the tarball was created.
* The date/timestamp, in ``YYYYMMDDHHMM`` format.
* The hash of the git commit from which the tarball was created.

For example, a snapshot tarball filename of
``openmpi-v2.x-201703070235-e4798fb.tar.bz2`` indicates that this tarball
was created from the v2.x branch, on March 7, 2017, at 2:35am GMT,
from git hash e4798fb.


Shared Library Version Number
-----------------------------

The `GNU Libtool official documentation
<https://www.gnu.org/software/libtool/manual/>`_ details how the
versioning scheme works.  The quick version is that the shared library
versions are a triple of integers: (current,revision,age), or
``c:r:a``.  This triple is not related to the Open MPI software
version number.  There are six simple rules for updating the values
(taken almost verbatim from the Libtool docs):

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

Here's how we apply those rules specifically to Open MPI:

#. The above rules do not apply to MCA components (a.k.a. "plugins");
   MCA component ``.so`` versions stay unspecified.
#. The above rules apply exactly as written to the following libraries
   starting with Open MPI version v1.5:

    * ``libopen-pal``
    * ``libmca_common_*``

#. The following libraries use a slightly modified version of the
   above rules: rules 4, 5, and 6 only apply to the official MPI and
   OpenSHMEM interfaces (functions, global variables).  The rationale
   for this decision is that the vast majority of our users only care
   about the official/public MPI/OpenSHMEM interfaces; we therefore
   want the ``.so`` version number to reflect only changes to the
   official MPI/OpenSHMEM APIs.  Put simply: non-MPI/OpenSHMEM API /
   internal changes to the MPI-application-facing libraries are
   irrelevant to pure MPI/OpenSHMEM applications.

   * ``libmpi``
   * ``libmpi_mpifh``
   * ``libmpi_usempi_tkr``
   * ``libmpi_usempi_ignore_tkr``
   * ``libmpi_usempif08``
   * ``libmpi_cxx``
   * ``libmpi_java``
   * ``liboshmem``

API and ABI Compatibility
-------------------------

Open MPI provides the following Application Programming Interface
(API) and Application Binary Interface (ABI) compatibility guarantees
for applications:

#. Open MPI is source code compatible (i.e., API compatible) across all
   versions.  This means that you can compile and link your compliant MPI
   application against :ref:`any version of Open MPI that supports the version
   of the MPI standard <release-notes-mpi-standard-conformance-label>` to
   which your application was written.

#. Open MPI provided forward application binary interface (ABI)
   compatibility within a major series for MPI applications starting
   with v1.3.2.  Prior to that version, no ABI guarantees were
   provided.

#. Open MPI reserves the right to break ABI compatibility at new major
   release series.

Open MPI |ompi_series| ABI compatibility
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Open MPI |ompi_series| series shared libraries are ABI compatible
with Open MPI v4.0.x and v4.1.x, with a few exceptions for Fortran.

* If your Fortran application was compiled in such a way that the size
  of an integer in C is different than the size of an integer in
  Fortran, you will need to rebuild and relink your application
  against Open MPI |ompi_series|.

* There are also Fortran API changes involving intents and
  ``ASYNCHRONOUS``, and some interfaces changed from named to unnamed.
  These may require changes to an application's source code, followed
  by recompilation and relinking.
