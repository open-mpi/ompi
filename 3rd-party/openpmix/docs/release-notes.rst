Release Notes
=============

The following abbreviated list of release notes applies to this code
base as of this writing (10 Mar 2023):

General notes
-------------

* Systems that have been tested are:

  * Linux (various flavors/distros), 64 bit (x86), with gcc, Intel,
    and Portland (*)
  * OS X (10.7 and above), 64 bit (x86_64), with gcc and clang (*)

* OpenPMIx has taken some steps towards Reproducible Builds
  (https://reproducible-builds.org/).  Specifically, OpenPMIx's
  ``configure`` and ``make`` process, by default, records the build date
  and some system-specific information such as the hostname where OpenPMIx
  was built and the username who built it.  If you desire a
  Reproducible Build, set the ``$SOURCE_DATE_EPOCH``, ``$USER`` and ``$HOSTNAME``
  environment variables before invoking ``configure`` and ``make``, and
  OpenPMIx will use those values instead of invoking ``whoami`` and/or
  ``hostname``, respectively.  See
  https://reproducible-builds.org/docs/source-date-epoch/ for
  information on the expected format and content of the
  ``$SOURCE_DATE_EPOCH`` variable.


Compiler Notes
--------------

* The Portland Group compilers prior to version 7.0 require the
  ``-Msignextend`` compiler flag to extend the sign bit when converting
  from a shorter to longer integer.  This is is different than other
  compilers (such as GNU).  When compiling PMIx with the Portland
  compiler suite, the following flags should be passed to PMIx's
  configure script:

  .. code-block:: sh

     shell$ ./configure CFLAGS=-Msignextend ...

  This will compile PMIx with the proper compile flags.

* Running on nodes with different endian and/or different datatype
  sizes within a single parallel job is supported in this release.
  However, PMIx does not resize data when datatypes differ in size
  (for example, sending a 4 byte double and receiving an 8 byte
  double will fail).
