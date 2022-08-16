.. _compiler-notes-section-label:

Compiler Notes
==============

* Open MPI requires a C99-capable compiler to build.

* On platforms other than x86-64, AArc64 (64-bit ARM), and PPC, Open
  MPI requires a compiler that either supports C11 atomics or the GCC
  ``__atomic`` atomics (e.g., GCC >= v4.8.x).

* 32-bit platforms are only supported with a recent compiler that
  supports C11 atomics. This includes GCC 4.9.x+ (although GCC 6.x or
  newer is recommended), the Intel compiler suite 16, and clang 3.1.

* Mixing compilers from different vendors when building Open MPI
  (e.g., using the C/C++ compiler from one vendor and the Fortran
  compiler from a different vendor) has been successfully employed by
  some Open MPI users (discussed on the Open MPI user's mailing list),
  but such configurations are not tested and not documented.  For
  example, such configurations may require additional compiler /
  linker flags to make Open MPI build properly.

  A not-uncommon case for this is when building on MacOS with the
  system-default GCC compiler (i.e., ``/usr/bin/gcc``), but a 3rd party
  gfortran (e.g., provided by Homebrew, in ``/usr/local/bin/gfortran``).
  Since these compilers are provided by different organizations, they
  have different default search paths.  For example, if Homebrew has
  also installed a local copy of Libevent (a 3rd party package that
  Open MPI requires), the MacOS-default ``gcc`` linker will find it
  without any additional command line flags, but the Homebrew-provided
  gfortran linker will not.  In this case, it may be necessary to
  provide the following on the configure command line:

  .. code-block:: sh

     shell$ ./configure FCFLAGS=-L/usr/local/lib ...

  This ``-L`` flag will then be passed to the Fortran linker when
  creating Open MPI's Fortran libraries, and it will therefore be able
  to find the installed Libevent.

* In general, the latest versions of compilers of a given vendor's
  series have the least bugs.  We have seen cases where Vendor XYZ's
  compiler version A.B fails to compile Open MPI, but version A.C
  (where C>B) works just fine.  If you run into a compile failure, you
  might want to double check that you have the latest bug fixes and
  patches for your compiler.

* Users have reported issues with older versions of the Fortran PGI
  compiler suite when using Open MPI's (non-default) ``--enable-debug``
  configure option.  Per the above advice of using the most recent
  version of a compiler series, the Open MPI team recommends using the
  latest version of the PGI suite, and/or not using the ``--enable-debug``
  configure option.  If it helps, here's what we have found with some
  (not comprehensive) testing of various versions of the PGI compiler
  suite:

  * pgi-8 : NO known good version with ``--enable-debug``
  * pgi-9 : 9.0-4 known GOOD
  * pgi-10: 10.0-0 known GOOD
  * pgi-11: NO known good version with ``--enable-debug``
  * pgi-12: 12.10 known BAD with ``-m32``, but known GOOD without ``-m32``
    (and 12.8 and 12.9 both known BAD with ``--enable-debug``)
  * pgi-13: 13.9 known BAD with ``-m32``, 13.10 known GOOD without ``-m32``
  * pgi-15: 15.10 known BAD with ``-m32``

* Similarly, there is a known Fortran PGI compiler issue with long
  source directory path names that was resolved in 9.0-4 (9.0-3 is
  known to be broken in this regard).

* Open MPI does not support the PGI compiler suite on OS X or MacOS.
  See issues below for more details:

  * https://github.com/open-mpi/ompi/issues/2604
  * https://github.com/open-mpi/ompi/issues/2605

* OpenSHMEM Fortran bindings do not support the "no underscore"
  Fortran symbol convention. IBM's ``xlf`` compilers build in that mode
  by default.  As such, IBM's ``xlf`` compilers cannot build/link the
  OpenSHMEM Fortran bindings by default. A workaround is to pass
  ``FC="xlf -qextname"`` at configure time to force a trailing
  underscore. See https://github.com/open-mpi/ompi/issues/3612 for
  more details.

* MPI applications that use the ``mpi_f08`` module on PowerPC platforms
  (tested ppc64le) will likely experience runtime failures if:

   * they are using a GNU linker (ld) version after v2.25.1 and before
     v2.28,
     *and*
   * they compiled with PGI (tested 17.5) or XL (tested v15.1.5)
     compilers.  This was noticed on Ubuntu 16.04 which uses the
     2.26.1 version of ``ld`` by default. However, this issue impacts
     any OS using a version of ``ld`` noted above. This GNU linker
     regression will be fixed in version 2.28.  `Here is a link to the
     GNU bug on this issue
     <https://sourceware.org/bugzilla/show_bug.cgi?id=21306>`_.  The
     XL compiler will include a fix for this issue in a future
     release.

* On NetBSD-6 (at least AMD64 and i386), and possibly on OpenBSD,
  Libtool misidentifies properties of f95/g95, leading to obscure
  compile-time failures if used to build Open MPI.  You can work
  around this issue by ensuring that libtool will not use f95/g95
  (e.g., by specifying ``FC=<some_other_compiler>``, or otherwise ensuring
  a different Fortran compiler will be found earlier in the path than
  ``f95``/``g95``), or by disabling the Fortran MPI bindings with
  ``--disable-mpi-fortran``.

* On OpenBSD/i386, if you configure with
  ``--enable-mca-no-build=patcher``, you will also need to add
  ``--disable-dlopen``.  Otherwise, odd crashes can occur
  nondeterministically.

* Absoft 11.5.2 plus a service pack from September 2012 (which Absoft
  says is available upon request), or a version later than 11.5.2
  (e.g., 11.5.3), is required to compile the Fortran ``mpi_f08``
  module.

* Open MPI does not support the Sparc v8 CPU target.  However,
  as of Solaris Studio 12.1, and later compilers, one should not
  specify ``-xarch=v8plus`` or ``-xarch=v9``.  The use of the options
  ``-m32`` and ``-m64`` for producing 32 and 64 bit targets, respectively,
  are now preferred by the Solaris Studio compilers.  GCC may
  require either ``-m32`` or ``-mcpu=v9 -m32``, depending on GCC version.

* If one tries to build OMPI on Ubuntu with Solaris Studio using the C++
  compiler and the ``-m32`` option, you might see a warning:

  .. code-block::

     CC: Warning: failed to detect system linker version, falling back to custom linker usage

  And the build will fail.  One can overcome this error by either
  setting ``LD_LIBRARY_PATH`` to the location of the 32 bit libraries
  (most likely ``/lib32``), or giving ``LDFLAGS="-L/lib32 -R/lib32"``
  to the ``configure`` command.  Officially, Solaris Studio is not
  supported on Ubuntu Linux distributions, so additional problems
  might occur.

* Open MPI does not support the ``gccfss`` compiler (GCC For SPARC
  Systems; a now-defunct compiler project from Sun).

* At least some versions of the Intel 8.1 compiler seg fault while
  compiling certain Open MPI source code files.  As such, it is not
  supported.

* It has been reported that the Intel 9.1 and 10.0 compilers fail to
  compile Open MPI on IA64 platforms.  As of 12 Sep 2012, there is
  very little (if any) testing performed on IA64 platforms (with any
  compiler).  Support is "best effort" for these platforms, but it is
  doubtful that any effort will be expended to fix the Intel 9.1 /
  10.0 compiler issuers on this platform.

* Early versions of the Intel 12.1 Linux compiler suite on x86_64 seem
  to have a bug that prevents Open MPI from working.  Symptoms
  including immediate segv of the wrapper compilers (e.g., ``mpicc``) and
  MPI applications.  As of 1 Feb 2012, if you upgrade to the latest
  version of the Intel 12.1 Linux compiler suite, the problem will go
  away.

* `Users have reported
  <https://github.com/open-mpi/ompi/issues/7615>`_ that the Intel
  Fortran compiler will fail to link Fortran-based MPI applications on
  macOS with linker errors similar to this:

  .. code-block:: text

     Undefined symbols for architecture x86_64:
       "_ompi_buffer_detach_f08", referenced from:
           import-atom in libmpi_usempif08.dylib
     ld: symbol(s) not found for architecture x86_64

  It appears that setting the environment variable
  ``lt_cx_ld_force_load=no`` before invoking Open MPI's ``configure``
  script works around the issue.  For example:

  .. code-block:: sh

     shell$ lt_cv_ld_force_load=no ./configure ...

* The Portland Group compilers prior to version 7.0 require the
  ``-Msignextend`` compiler flag to extend the sign bit when converting
  from a shorter to longer integer.  This is different than other
  compilers (such as GNU).  When compiling Open MPI with the Portland
  compiler suite, the following flags should be passed to Open MPI's
  ``configure`` script:

  .. code-block:: sh

     shell$ ./configure CFLAGS=-Msignextend CXXFLAGS=-Msignextend \
            --with-wrapper-cflags=-Msignextend \
            --with-wrapper-cxxflags=-Msignextend ...

  This will both compile Open MPI with the proper compile flags and
  also automatically add ``-Msignextend`` when the C and C++ MPI wrapper
  compilers are used to compile user MPI applications.

* It has been reported that Pathscale 5.0.5 and 6.0.527 compilers
  give an internal compiler error when trying to build Open MPI.

* As of July 2017, the Pathscale compiler suite apparently has no
  further commercial support, and it does not look like there will be
  further releases.  Any issues discovered regarding building /
  running Open MPI with the Pathscale compiler suite therefore may not
  be able to be resolved.

* Using the Absoft compiler to build the MPI Fortran bindings on Suse
  9.3 is known to fail due to a Libtool compatibility issue.

* There is now only a single Fortran MPI wrapper compiler and a
  single Fortran OpenSHMEM wrapper compiler: ``mpifort`` and ``oshfort``,
  respectively.

  .. caution:: The legacy executable names ``mpif77`` and ``mpif90``
               still exist, but they are symbolic links to
               ``mpifort``.  Users should immediately stop using the
               legacy names, and should always use ``mpifort``.

  Similarly, Open MPI's ``configure`` script only recognizes the ``FC``
  and ``FCFLAGS`` environment variables (to specify the Fortran
  compiler and compiler flags, respectively).  The ``F77`` and ``FFLAGS``
  environment variables are **IGNORED**.

  .. important:: As a direct result, it is **STRONGLY** recommended
     that you specify a Fortran compiler that uses file suffixes to
     determine Fortran code layout (e.g., free form vs. fixed).  For
     example, with some versions of the IBM XLF compiler, it is
     preferable to use ``FC=xlf`` instead of ``FC=xlf90``, because
     ``xlf`` will automatically determine the difference between free
     form and fixed Fortran source code.

  However, many Fortran compilers allow specifying additional
  command-line arguments to indicate which Fortran dialect to use.
  For example, if ``FC=xlf90``, you may need to use ``mpifort --qfixed ...``
  to compile fixed format Fortran source files.

  You can use either ``ompi_info`` or ``oshmem_info`` to see with which
  Fortran compiler Open MPI was configured and compiled.

  There are up to three sets of Fortran MPI bindings that may be
  provided (depending on your Fortran compiler):

  #. ``mpif.h``: This is the first MPI Fortran interface that was
     defined in MPI-1.  It is a file that is included in Fortran
     source code.  The only interface declared in Open MPI's
     ``mpif.h`` is ``MPI_SIZEOF`` (because of its polymorphism).  All
     other interfaces are implicit.

  #. ``mpi`` module: The ``mpi`` module file was added in MPI-2.  It
     provides strong compile-time parameter type checking for MPI all
     interfaces.

  #. ``mpi_f08`` module: The ``mpi_f08`` module was added in MPI-3.
     It provides many advantages over the ``mpif.h`` file and ``mpi``
     module.  For example, MPI handles have distinct types (vs. all
     being integers).  See the `MPI-3.0 (or later) standard
     <https://www.mpi-forum.org/docs/>`_ for more details.

  .. important:: The ``mpi_f08`` module is **STRONGLY** recommended
     for all new MPI Fortran subroutines and applications.  Note that
     the ``mpi_f08`` module can be used in conjunction with the other
     two Fortran MPI bindings in the same application (only one
     binding can be used per subroutine/function, however).  Full
     interoperability between ``mpif.h``/``mpi`` module and
     ``mpi_f08`` module MPI handle types is provided, allowing
     ``mpi_f08`` to be used in new subroutines in legacy MPI
     applications.

  Per the OpenSHMEM specification, there is only one Fortran OpenSHMEM
  binding provided:

  * ``shmem.fh``: All Fortran OpenSHMEM programs should include
    ``shmem.f``, and Fortran OpenSHMEM programs that use constants
    defined by OpenSHMEM **MUST** include ``shmem.fh``.

  The following notes apply to the above-listed Fortran bindings:

  * All Fortran compilers support the ``mpif.h``/``shmem.fh``-based
    bindings, with one exception: the ``MPI_SIZEOF`` interfaces will
    only be present when Open MPI is built with a Fortran compiler
    that supports the ``INTERFACE`` keyword and ``ISO_FORTRAN_ENV``.  Most
    notably, this excludes the GNU Fortran compiler suite before
    version 4.9.

  * The level of support provided by the ``mpi`` module is based on your
    Fortran compiler.

    If Open MPI is built with a non-GNU Fortran compiler, or if Open
    MPI is built with the GNU Fortran compiler >= v4.9, all MPI
    subroutines will be prototyped in the ``mpi`` module.  All calls to
    MPI subroutines will therefore have their parameter types checked
    at compile time.

    If Open MPI is built with an old ``gfortran`` (i.e., < v4.9), a
    limited ``mpi`` module will be built.  Due to the limitations of
    these compilers, and per guidance from the MPI-3.0 (and later)
    specification, all MPI subroutines with "choice" buffers are
    specifically *not* included in the ``mpi`` module, and their
    parameters will not be checked at compile time.  Specifically, all
    MPI subroutines with no "choice" buffers are prototyped and will
    receive strong parameter type checking at run-time (e.g.,
    ``MPI_INIT``, ``MPI_COMM_RANK``, etc.).

    Similar to the ``mpif.h`` interface, ``MPI_SIZEOF`` is only
    supported on Fortran compilers that support ``INTERFACE`` and
    ``ISO_FORTRAN_ENV``.

  * The ``mpi_f08`` module has been tested with the Intel Fortran
    compiler and gfortran >= 4.9.  Other modern Fortran compilers
    likely also work.

    Many older Fortran compilers do not provide enough modern Fortran
    features to support the ``mpi_f08`` module.  For example, ``gfortran``
    < v4.9 does provide enough support for the ``mpi_f08`` module.

  You can examine the output of the following command to see all
  the Fortran features that are/are not enabled in your Open MPI
  installation:

  .. code-block:: sh

     shell$ ompi_info | grep -i fort
