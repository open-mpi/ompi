Open MPI v5.0.x series
======================

This file contains all the NEWS updates for the Open MPI v5.0.x
series, in reverse chronological order.

Open MPI version 5.0.0rc6
-------------------------
:Date: 15 April 2022

.. admonition:: MPIR API has been removed
   :class: warning

   As was announced in summer 2017, Open MPI has removed support of
   MPIR-based tools beginning with the release of Open MPI v5.0.0.

   The new PRRTE based runtime environment supports PMIx-tools API
   instead of the legacy MPIR API for debugging parallel jobs.

   See https://github.com/openpmix/mpir-to-pmix-guide for more
   information.

.. admonition:: zlib is suggested for better user experience
   :class: note

   PMIx will optionally use zlib to compress large data streams.
   This may result in faster startup times and
   smaller memory footprints (compared to not using compression).
   The Open MPI community recommends building zlib support with PMIx,
   regardless of whether you are using an externally-installed PMIx or
   the PMIx that is installed with Open MPI.

.. caution::
   Open MPI no longer builds 3rd-party packages
   such as Libevent, HWLOC, PMIx, and PRRTE as MCA components
   and instead:
      
   #. Relies on external libraries whenever possible, and
   #. Builds the 3rd party libraries only if needed, and as independent
      libraries, rather than linked into the Open MPI core libraries.


Changes since rc5:

  - The PRRTE submodule pointer has been updated to bring in the following fixes:

    - Fixed a bug where the deprecated option ``--oversubscribe`` for ``mpirun``
      was not translated correctly to its new equivalent (``--map-by :oversubscribe``).
    - Fixed a case where ``--map-by ppr:x:oversubscribe`` would not work correctly.
      In this case, ``:oversubscribe`` was effectively ignored.

  - Fixed incorrect behavior with ``MPI_Allreduce()`` when using ``MPI_MAX`` with
    the ``MPI_UNSIGNED_LONG`` type. Thanks to Kendra Long for the report and their
    contrubution to the fix.

  - Various fixes to the ``openmpi.spec`` file to fix issues with rpm generation.

  - Fixed a bug in one-sided ``UCX`` calls where not all in-flight messages
    would be flushed before cleanup.

  - Build fixes - the following builds options with Open MPI were fixed:

    - ``usNIC`` - (``configure --with-usnic=..``)
    - ``HCOLL`` - (``configure --with-hcoll=..``)
    - ``XPMEM`` - (``configure --with-xpmem=..``).

      - Thanks to Alex Margol for the fix.

  - Various documentation improvements and updates.

- Updated PMIx to the ``v4.2`` branch - current hash: ``7ddb00e``.
- Updated PRRTE to the ``v2.1`` branch - current hash: ``407e8d5``.

- New Features:

  - ULFM Fault Tolerance support has been added. See :ref:`the ULFM section <ulfm-label>`
  - ``CUDA`` is now supported in the ``ofi`` MTL.
  - mpirun option ``--mca ompi_display_comm mpi_init``/``mpi_finalize``
    has been added. This enables a communication protocol report:
    when ``MPI_Init`` is invoked (using the ``mpi_init`` value) and/or
    when ``MPI_Finalize`` is invoked (using the ``mpi_finalize`` value).
  - The threading framework has been added to allow building OMPI with different
    threading libraries. It currently supports Argobots, Qthreads, and Pthreads.
    See the ``--with-threads`` option in the ``configure`` command.
    Thanks to Shintaro Iwasaki and Jan Ciesko for their contributions to
    this effort.
  - New Thread Local Storage API: Removes global visibility of TLS structures
    and allows for dynamic TLS handling.
  - Added load-linked, store-conditional atomics support for AArch64.
  - Added atomicity support to the ``ompio`` component.
  - Added support for MPI minimum alignment key to the one-sided ``RDMA`` component.
  - Add ability to detect patched memory to ``memory_patcher``. Thanks
    to Rich Welch for the contribution.

- MPI-4.0 updates and additions:

  - Support for ``MPI Sesisons`` has been added.
  - Added partitioned communication using persistent sends
    and persistent receives.
  - Added persistent collectives to the ``MPI_`` namespace
    (they were previously available via the ``MPIX_`` prefix).
  - Added ``MPI_Isendrecv()`` and its variants.
  - Added support for ``MPI_Comm_idup_with_info()``.
  - Added support for ``MPI_Info_get_string()``.
  - Added support for ``initial_error_handler`` and the ``ERRORS_ABORT`` infrastructure.
  - Added error handling for "unbound" errors to ``MPI_COMM_SELF``.
  - Made ``MPI_Comm_get_info()``, ``MPI_File_get_info()``, and
    ``MPI_Win_get_info()`` compliant to the standard.
  - Droped unknown/ignored info keys on communicators, files,
    and windows.

- Transport updates and improvements

  - One-sided Communication:

    - Many MPI one-sided and RDMA emulation fixes for the ``tcp`` BTL.

      - This patch series fixs many issues when running with
        ``--mca osc rdma --mca btl tcp``, IE - TCP support for one sided
        MPI calls.
    - Many MPI one-sided fixes for the ``ucx`` BTL.
    - Added support for ``acc_single_intrinsic`` to the one-sided ``ucx`` component.
    - Removed the legacy ``pt2pt`` one-sided component. Users should use
      the ``rdma`` one-sided component instead with the ``tcp`` BTL and/or other BTLs
      to use MPI one sided-calls via TCP transport.

  - Updated the ``tcp`` BTL to use graph solving for global
    interface matching between peers in order to improve ``MPI_Init()`` wireup
    performance.

  - Changes to the BTL ``OFI`` component to better support the HPE SS11 network.

  - Shared Memory:

    - The legacy ``sm`` (shared memory) BTL has been removed.
      The next-generation shared memory BTL ``vader`` replaces it,
      and has been renamed to be ``sm`` (``vader`` will still work as an alias).
    - Update the new ``sm`` BTL to not use Linux Cross Memory Attach (CMA) in user namespaces.
    - Fixed a crash when using the new ``sm`` BTL when compiled with Linux Cross Memory Attach (``XPMEM``).
      Thanks to George Katevenis for reporting this issue.

  - Updated the ``-mca pml`` option to only accept one pml, not a list.

- Deprecations and removals:

  - ORTE, the underlying OMPI launcher has been removed, and replaced
    with The PMIx Reference RunTime Environment (``PRTE``).
  - PMI support has been removed from Open MPI; now only PMIx is supported.
    Thanks to Zach Osman for removing config/opal_check_pmi.m4.
  - Removed transports PML ``yalla``, ``mxm``, MTL ``psm``, and ``ikrit`` components.
    These transports are no longer supported, and are replaced with ``UCX``.
  - Removed all vestiges of Checkpoint Restart (C/R) support.
  - 32 bit atomics are now only supported via C11 compliant compilers.
  - Explicitly disable support for GNU gcc < v4.8.1 (note: the default
    gcc compiler that is included in RHEL 7 is v4.8.5).
  - Various atomics support removed: S390/s390x, Sparc v9, ARMv4 and ARMv5 with CMA
    support.
  - The MPI C++ bindings have been removed.
  - The mpirun options ``--am`` and ``--amca`` options have been deprecated.
  - ompi/contrib: Removed ``libompitrace``.
    This library was incomplete and unmaintained. If needed, it
    is available in the v4/v4.1 series.

- HWLOC updates:

  - Open MPI now requires HWLOC v1.11.0 or later.
  - The internal HWLOC shipped with OMPI has been updated to v2.7.1.
  - Enable --enable-plugins when appropriate.

- Documentation updates and improvements:

  - Open MPI now uses readthedocs.io for all documentation.
  - Converted man pages to markdown. Thanks to Fangcong Yin for their contribution
    to this effort.
  - Various ``README.md`` and ``HACKING.md`` fixes - thanks to: Yixin Zhang, Samuel Cho,
    Robert Langfield, Alex Ross, Sophia Fang, mitchelltopaloglu, Evstrife, Hao Tong
    and Lachlan Bell for their contributions.
  - Various CUDA documentation fixes. Thanks to Simon Byrne for finding
    and fixing these typos.

- Build updates and fixes:

  - Various changes and cleanup to fix, and better support the static building of Open MPI.
  - Change the default component build behavior to prefer building
    components as part of the core Open MPI library instead of individual DSOs.
    Currently, this means the Open SHMEM layer will only build if
    the UCX library is found.
  - ``autogen.pl`` now supports a ``-j`` option to run multi-threaded.
    Users can also use the environment variable ``AUTOMAKE_JOBS``.
  - Updated ``autogen.pl`` to support macOS Big Sur. Thanks to
    @fxcoudert for reporting the issue.
  - Fixed bug where ``autogen.pl`` would not ignore all
    excluded components when using the ``--exclude`` option.
  - Fixed a bug the ``-r`` option of ``buildrpm.sh`` which would result
    in an rpm build failure. Thanks to John K. McIver III for reporting and fixing.
  - Removed the ``C++`` compiler requirement to build Open MPI.
  - Updates to improve the handling of the compiler version string in the build system.
    This fixes a compiler error with clang and armclang.
  - Added OpenPMIx binaries to the build, including ``pmix_info``.
    Thanks to Mamzi Bayatpour for their contribution to this effort.
  - Open MPI now links to Libevent using ``-levent_core``
    and ``-levent_pthread`` instead of ``-levent``.
  - Added support for setting the wrapper C compiler.
    This adds a new option: ``--with-wrapper-cc=`` to the ``configure`` command.
  - Fixed compilation errors when running on IME file systems
    due to a missing header inclusion. Thanks to Sylvain Didelot for finding
    and fixing this issue.
  - Add support for GNU Autoconf v2.7.x.

- Other updates and bug fixes:

  - Updated Open MPI to use ``ROMIO`` v3.4.1.
  - Fixed Fortran-8-byte-INTEGER vs. C-4-byte-int issue in the ``mpi_f08``
    MPI Fortran bindings module. Thanks to @ahaichen for reporting the bug.
  - Add missing ``MPI_Status`` conversion subroutines:
    ``MPI_Status_c2f08()``, ``MPI_Status_f082c()``, ``MPI_Status_f082f()``,
    ``MPI_Status_f2f08()`` and the ``PMPI_*`` related subroutines.
  - Fixed Fortran keyword issue when compiling ``oshmem_info``.
    Thanks to Pak Lui for finding and fixing the bug.
  - Added check for Fortran ``ISO_FORTRAN_ENV:REAL16``. Thanks to
    Jeff Hammond for reporting this issue.
  - Fixed Fortran preprocessor issue with CPPFLAGS.
    Thanks to Jeff Hammond for reporting this issue.
  - MPI module: added the mpi_f08 TYPE(MPI_*) types for Fortran.
    Thanks to George Katevenis for the report and their contribution to the patch.
  - Fixed a typo in an error string when showing the stackframe. Thanks to
    Naribayashi Akira for finding and fixing the bug.
  - Fixed output error strings and some comments in the Open MPI code base.
    Thanks to Julien Emmanuel for finding and fixing these issues.
  - The ``uct`` BTL transport now supports ``UCX`` v1.9 and higher.
    There is no longer a maximum supported version.
  - Updated the UCT BTL defaults to allow Mellanox HCAs
    (``mlx4_0``, and ``mlx5_0``) for compatibility with the one-sided ``rdma`` component.
  - Fixed a crash during CUDA initialization.
    Thanks to Yaz Saito for finding and fixing the bug.
  - Singleton ``MPI_Comm_spawn()`` support has been fixed.
  - PowerPC atomics: Force usage of ppc assembly by default.
  - Various datatype bugfixes and performance improvements.
  - Various pack/unpack bugfixes and performance improvements.
  - Various OSHMEM bugfixes and performance improvements.
  - New algorithm for Allgather and Allgatherv has been added, based on the
    paper *"Sparbit: a new logarithmic-cost and data locality-aware MPI
    Allgather algorithm"*. Default algorithm selection rules are
    un-changed, to use these algorithms add:
    ``--mca coll_tuned_allgather_algorithm sparbit`` and/or
    ``--mca coll_tuned_allgatherv_algorithm sparbit`` to your ``mpirun`` command.
    Thanks to: Wilton Jaciel Loch, and Guilherme Koslovski for their contribution.
  - Updated the usage of .gitmodules to use relative paths from
    absolute paths. This allows the submodule cloning to use the same
    protocol as OMPI cloning. Thanks to Felix Uhl for the contribution.
