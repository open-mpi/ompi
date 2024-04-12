.. _compiler-notes-section-label:

Compiler Notes
==============

* PMIx requires a C99-capable compiler to build.

* PMIx requires a compiler that either supports C11 atomics or the GCC
  ``__atomic`` atomics (e.g., GCC >= v4.8.x).

* 32-bit platforms are not currently supported.

* In general, the latest versions of compilers of a given vendor's
  series have the least bugs.  We have seen cases where Vendor XYZ's
  compiler version A.B fails to compile PMIx, but version A.C
  (where C>B) works just fine.  If you run into a compile failure, you
  might want to double check that you have the latest bug fixes and
  patches for your compiler.

* PMIx does not support the PGI compiler suite on OS X or MacOS.
  See issues below for more details:

  * https://github.com/open-mpi/ompi/issues/2604
  * https://github.com/open-mpi/ompi/issues/2605

* At least some versions of the Intel 8.1 compiler seg fault while
  compiling certain PMIx source code files.  As such, it is not
  supported.

* It has been reported that the Intel 9.1 and 10.0 compilers fail to
  compile PMIx on IA64 platforms.  As of 12 Sep 2012, there is
  very little (if any) testing performed on IA64 platforms (with any
  compiler).  Support is "best effort" for these platforms, but it is
  doubtful that any effort will be expended to fix the Intel 9.1 /
  10.0 compiler issuers on this platform.

* Early versions of the Intel 12.1 Linux compiler suite on x86_64 seem
  to have a bug that prevents PMIx from working.  Symptoms
  including immediate segv of the wrapper compiler (``pmixcc``) and
  PMIx-based applications.  If you upgrade to the latest
  version of the Intel 12.1 Linux compiler suite, the problem will go
  away.

* The Portland Group compilers prior to version 7.0 require the
  ``-Msignextend`` compiler flag to extend the sign bit when converting
  from a shorter to longer integer.  This is different than other
  compilers (such as GNU).  When compiling PMIx with the Portland
  compiler suite, the following flags should be passed to PMIx's
  ``configure`` script:

  .. code-block:: sh

     shell$ ./configure CFLAGS=-Msignextend \
            --with-wrapper-cflags=-Msignextend ...

  This will both compile PMIx with the proper compile flags and
  also automatically add ``-Msignextend`` when the C wrapper
  compiler is used to compile user PMIx-based applications.

* It has been reported that Pathscale 5.0.5 and 6.0.527 compilers
  give an internal compiler error when trying to build PMIx.

* As of July 2017, the Pathscale compiler suite apparently has no
  further commercial support, and it does not look like there will be
  further releases.  Any issues discovered regarding building /
  running PMIx with the Pathscale compiler suite therefore may not
  be able to be resolved.
