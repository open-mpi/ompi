Open MPI v5.0.x series
======================

This file contains all the NEWS updates for the Open MPI v5.0.x
series, in reverse chronological order.

Open MPI version 5.0.0rc14
--------------------------
:Date: 17 October 2023

.. admonition:: The MPIR API has been removed
   :class: warning

   As was announced in the summer of 2017, Open MPI has removed
   support for MPIR-based tools beginning with the release of Open MPI
   v5.0.0.

   Open MPI now uses the `PRRTE <https://github.com/openpmix/prrte>`_
   runtime environment, which supports the `PMIx <https://pmix.org/>`_
   tools API |mdash| instead of the legacy MPIR API |mdash| for
   debugging parallel jobs.

   Users who still need legacy MPIR support should see
   https://github.com/hpc/mpir-to-pmix-guide for more information.

.. admonition:: Zlib is suggested for better performance
   :class: note

   `PMIx <https://pmix.org/>`_ will optionally use `Zlib
   <https://github.com/madler/zlib>`_ to compress large data streams.
   This may result in faster startup times and smaller memory
   footprints (compared to not using compression).

   The Open MPI community recommends building PMIx with Zlib support,
   regardless of whether you are using an externally-installed PMIx or
   the bundled PMIx that is included with Open MPI distribution
   tarballs.

   Note that while the Zlib library *may* be present on many systems
   by default, the Zlib header files |mdash| which are needed to build
   PMIx with Zlib support |mdash| may need to be installed separately
   before building PMIx.

.. caution:: Open MPI has changed the default behavior of how it
             builds and links against its :ref:`required 3rd-party
             packages <label-install-required-support-libraries>`:
             `Libevent <https://libevent.org/>`_, `Hardware Locality
             <https://www.open-mpi.org/projects/hwloc/>`_, `PMIx
             <https://pmix.org/>`_, and `PRRTE
             <https://github.com/openpmix/prrte>`_.

             #. Unlike previous versions of Open MPI, Open MPI 5.0 and
                later will prefer an external package that meets our
                version requirements, even if it is older than our
                internal version.
             #. To simplify managing dependencies, any required
                packages that Open MPI |ompi_series| bundles will be
                installed in Open MPI's installation prefix, without
                name mangling.

                For example, if a valid Libevent installation cannot
                be found and Open MPI therefore builds its bundled
                version, a ``libevent.so`` will be installed in Open
                MPI's installation tree. This is different from
                previous releases, where Open MPI name-mangled the
                Libevent symbols and then statically pulled the
                library into ``libmpi.so``.

- Changes since rc13:

  - Update PMIx to hash: ``f8f578392ec77dd7a1d76ca697da4f15afcb0161``.
  - Update PRRTE to hash: ``bb4085053a0b268ae2a2e04ed56387f53e4a3e7a``.
  - Documentation updates
  - Fix build case with --disable-prrte
  - Update PRRTe and PMIx pointers to pull in fixes, including spurious log messages, and also
    RPM fixes.
  - pcomm: fix fortran interface for precv/psend.
  - Fix UCX support level check.
  - Add support for MPI_ERR_VALUE_TOO_LARGE
  - ofi - add MCA parameters to not use FI_HMEM
    This commit adds two MCA parameters:
    mtl_ofi_disable_hmem
    btl_ofi_disable_hmem
  - oshmem:
    Add symmetric remote key handling
    Fixed DEVICE_NIC_MEM support to use RDMA memory type.
  - Fix a small issue in properly setting filename when building the empty schizo rst file.

- All other notable updates for v5.0.0:

  - New Features:

    - ULFM Fault Tolerance support has been added. See :ref:`the ULFM
      section <ulfm-label>`.
    - CUDA is now supported in the ``ofi`` MTL.
    - New MCA parameter ``ompi_display_comm``, enabling a
      communication report.  When set to ``mpi_init``, display the
      report when ``MPI_Init()`` is invoked.  When set to
      ``mpi_finalize``, display the report during ``MPI_Finalize()``.
    - A threading framework has been added to allow building Open MPI
      with different threading libraries. It currently supports
      `Argobots <https://www.argobots.org/>`_, `Qthreads
      <https://github.com/Qthreads/qthreads>`_, and Pthreads.  See the
      ``--with-threads`` option in the ``configure`` command.  Thanks
      to Shintaro Iwasaki and Jan Ciesko for their contributions to
      this effort.
    - New Thread Local Storage API: Removes global visibility of TLS
      structures and allows for dynamic TLS handling.
    - Added new ``Accelerator`` framework. CUDA-specific code
      was replaced with a generic framework that standardizes various
      device features such as copies or pointer type detection. This
      allows for modularized implementation of various devices such as
      the newly introduced ROCm Accelerator component. The redesign
      also allows for Open MPI builds to be shipped with CUDA
      support enabled without requiring CUDA libraries.
    - Added load-linked, store-conditional atomics support for
      AArch64.
    - Added atomicity support to the ``ompio`` component.
    - ``osc/rdma``: Added support for MPI minimum alignment key.
    - Add ability to detect patched memory to
      ``memory_patcher``. Thanks to Rich Welch for the contribution.
    - ``coll/ucc``: Added support for the ``MPI_Scatter()`` and
      ``MPI_Iscatter()`` collectives.

  - MPI-4.0 updates and additions:

    - Support for MPI Sessions has been added.
    - Added partitioned communication using persistent sends
      and persistent receives.
    - Added persistent collectives to the ``MPI_`` namespace
      (they were previously available via the ``MPIX_`` prefix).
    - Added ``MPI_Isendrecv()`` and its variants.
    - Added support for ``MPI_Comm_idup_with_info()``.
    - Added support for ``MPI_Info_get_string()``.
    - Added support for ``initial_error_handler`` and the
      ``ERRORS_ABORT`` infrastructure.
    - Added error handling for unbound errors to ``MPI_COMM_SELF``.
    - Made ``MPI_Comm_get_info()``, ``MPI_File_get_info()``, and
      ``MPI_Win_get_info()`` compliant to the standard.
    - Droped unknown/ignored info keys on communicators, files,
      and windows.
    - Initial implementations of ``MPI_COMM_TYPE_HW_GUIDED`` and
      ``MPI_COMM_TYPE_HW_GUIDED`` added.
    - ``MPI_Info_get()`` and ``MPI_Info_get_valuelen()`` are now
      deprecated.
    - Issue a deprecation warning when ``MPI_Cancel()`` is called for
      a non-blocking send request.

  - Transport updates and improvements

    - One-sided Communication:

      - Many MPI one-sided and RDMA emulation fixes for the ``tcp`` BTL.

        This patch series fixs many issues when running with ``--mca
        osc rdma --mca btl tcp``, i.e., TCP support for one sided
        MPI calls.

      - Many MPI one-sided fixes for the ``uct`` BTL.
      - Added support for ``acc_single_intrinsic`` to the one-sided
        ``ucx`` component.
      - Removed the legacy ``pt2pt`` one-sided component. Users should
        now utilize the ``rdma`` one-sided component instead.  The
        ``rdma`` component will use BTL components |mdash| such as the
        TCP BTL |mdash| to effect one-sided communications.

    - Updated the ``tcp`` BTL to use graph solving for global
      interface matching between peers in order to improve
      ``MPI_Init()`` wireup performance.

    - OFI

      - Improved support for the HPE SS11 network.
      - Added cache bypass mechanism. This fixes conflicts with
        `Libfabric <https://libfabric.org/>`_, which has its own
        registration cache. This adds a bypass flag which can be used
        for providers known to have their own registration cache.

    - Shared Memory:

      - The legacy ``sm`` (shared memory) BTL has been removed.  The
        next-generation shared memory BTL ``vader`` replaces it, and
        has been renamed to be ``sm`` (``vader`` will still work as an
        alias).
      - Update the new ``sm`` BTL to not use Linux Cross Memory Attach
        (CMA) in user namespaces.
      - Fixed a crash when using the new ``sm`` BTL when compiled with
        Linux Cross Memory Attach (``XPMEM``).  Thanks to George
        Katevenis for reporting this issue.

    - Updated the ``-mca pml`` option to only accept one PML, not a list.

  - Deprecations and removals:

    - ORTE, the underlying Open MPI launcher has been removed, and
      replaced with the `PMIx Reference RunTime Environment
      <https://github.com/openpmix/prrte>`_ (``PRTE``).
    - PMI support has been removed from Open MPI; now only PMIx is
      supported.  Thanks to Zach Osman for contributing.
    - The following components have been removed, and are replaced by
      UCX support: PML ``yalla``, PML ``mxm``, SPML ``ikrit``.
    - The MTL ``psm`` component has been removed and is no longer
      supported.
    - Removed all vestiges of Checkpoint Restart (C/R) support.
    - 32 bit atomics are now only supported via C11 compliant compilers.
    - Explicitly disable support for GNU gcc < v4.8.1 (note: the
      default gcc compiler that is included in RHEL 7 is v4.8.5).
    - Various atomics support removed: S390/s390x, Sparc v9, ARMv4 and
      ARMv5 with CMA support.
    - The MPI C++ bindings have been removed.
    - The ``mpirun`` options ``--am`` and ``--amca`` options have been
      deprecated.
    - The ``libompitrace`` contributed library has been removed.
      This library was incomplete and unmaintained. If needed, it
      is available in the v4.x series.
    - The rankfile format no longer supports physical processor
      locations. Only logical processor locations are supported.
    - 32-bit builds have been disabled. Building Open MPI in a 32-bit
      environment is no longer supported.  32 bit support is still
      available in the v4.x series.

  - Hardware Locality updates:

    - Open MPI now requires Hardware Locality v1.11.0 or later.
    - The internally-bundled Hardware Locality shipped with Open MPI
      has been updated to v2.7.1.
    - Open MPI builds Hardware Locality with ``--enable-plugins`` when
      appropriate.

  - Documentation updates and improvements:

    - Open MPI has consolidated and converted all of its documentation
      to use `ReStructured Text
      <https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html>`_
      and `Sphinx <https://www.sphinx-doc.org/>`_.

      - The resulting documentation is now hosted on
        https://docs.open-mpi.org (via `ReadTheDocs
        <https://ReadTheDocs.io/>`_).
      - The documentation is also wholly available offline via Open
        MPI distribution tarballs, in the ``docs/_build/html``
        directory.

    - Many, many people from the Open MPI community contributed to the
      overall documentation effort |mdash| not only those who are
      listed in the Git commit logs |mdash| including (but not limited
      to):

      - Lachlan Bell
      - Simon Byrne
      - Samuel Cho
      - Tony Curtis
      - Lisandro Dalcin
      - Sophia Fang
      - Rick Gleitz
      - Colton Kammes
      - Quincey Koziol
      - Robert Langfield
      - Nick Papior
      - Luz Paz
      - Alex Ross
      - Hao Tong
      - Mitchell Topaloglu
      - Siyu Wu
      - Fangcong Yin
      - Seth Zegelstein
      - Yixin Zhang
      - William Zhang

  - Build updates and fixes:

    - Various changes and cleanup to fix, and better support the
      static building of Open MPI.
    - Change the default component build behavior to prefer building
      components as part of the core Open MPI library instead of
      individual DSOs.  Currently, this means the Open SHMEM layer
      will only build if the UCX library is found.
    - ``autogen.pl`` now supports a ``-j`` option to run
      multi-threaded.  Users can also use the environment variable
      ``AUTOMAKE_JOBS``.
    - Updated ``autogen.pl`` to support macOS Big Sur. Thanks to
      @fxcoudert for reporting the issue.
    - Fixed bug where ``autogen.pl`` would not ignore all excluded
      components when using the ``--exclude`` option.
    - Fixed a bug the ``-r`` option of ``buildrpm.sh`` which would
      result in an rpm build failure. Thanks to John K. McIver III for
      reporting and fixing.
    - Removed the ``C++`` compiler requirement to build Open MPI.
    - Updates to improve the handling of the compiler version string
      in the build system.  This fixes a compiler error with clang and
      armclang.
    - Added OpenPMIx binaries to the build, including ``pmix_info``.
      Thanks to Mamzi Bayatpour for their contribution to this effort.
    - Open MPI now links to Libevent using ``-levent_core``
      and ``-levent_pthread`` instead of ``-levent``.
    - Added support for setting the wrapper C compiler.  This adds a
      new option: ``--with-wrapper-cc=NAME`` to the ``configure`` command.
    - Fixed compilation errors when running on IME file systems due to
      a missing header inclusion. Thanks to Sylvain Didelot for
      finding and fixing this issue.
    - Add support for GNU Autoconf v2.7.x.

  - Other updates and bug fixes:

    - Updated Open MPI to use ``ROMIO`` v3.4.1.
    - ``common/ompio``: implement pipelined read and write operation.
      This new new code path shows significant performance
      improvements for reading/writing device buffers compared to the
      previous implementation, and reduces the memory footprint of
      Open MPI IO ("OMPIO") by allocating smaller temporary buffers.
    - Fixed Fortran-8-byte-INTEGER vs. C-4-byte-int issue in the
      ``mpi_f08`` MPI Fortran bindings module. Thanks to @ahaichen for
      reporting the bug.
    - Add missing ``MPI_Status`` conversion subroutines:
      ``MPI_Status_c2f08()``, ``MPI_Status_f082c()``,
      ``MPI_Status_f082f()``, ``MPI_Status_f2f08()`` and the
      ``PMPI_*`` related subroutines.
    - Fixed Fortran keyword issue when compiling ``oshmem_info``.
      Thanks to Pak Lui for finding and fixing the bug.
    - Added check for Fortran ``ISO_FORTRAN_ENV:REAL16``. Thanks to
      Jeff Hammond for reporting this issue.
    - Fixed Fortran preprocessor issue with ``CPPFLAGS``.
      Thanks to Jeff Hammond for reporting this issue.
    - MPI module: added the ``mpi_f08`` ``TYPE(MPI_*)`` types for
      Fortran.  Thanks to George Katevenis for the report and their
      contribution to the patch.
    - Fixed a typo in an error string when showing the stack
      frame. Thanks to Naribayashi Akira for finding and fixing the
      bug.
    - Fixed output error strings and some comments in the Open MPI
      code base.  Thanks to Julien Emmanuel for tirelessly finding and
      fixing these issues.
    - The ``uct`` BTL transport now supports ``UCX`` v1.9 and higher.
      There is no longer a maximum supported version.
    - Updated the UCT BTL defaults to allow NVIDIA/Mellanox HCAs
      (``mlx4_0``, and ``mlx5_0``) for compatibility with the
      one-sided ``rdma`` component.
    - Fixed a crash during CUDA initialization.
      Thanks to Yaz Saito for finding and fixing the bug.
    - Singleton ``MPI_Comm_spawn()`` support has been fixed.
    - PowerPC atomics: Force usage of ppc assembly by default.
    - The default atomics have been changed to be GCC, with C11 as a
      fallback. C11 atomics incurs sequential memory ordering, which
      in most cases is not desired.
    - Various datatype bugfixes and performance improvements.
    - Various pack/unpack bugfixes and performance improvements.
    - Various OSHMEM bugfixes and performance improvements.
    - New algorithm for Allgather and Allgatherv has been added, based
      on the paper *"Sparbit: a new logarithmic-cost and data
      locality-aware MPI Allgather algorithm"*. Default algorithm
      selection rules are unchanged; to use these algorithms add:
      ``--mca coll_tuned_allgather_algorithm sparbit`` and/or ``--mca
      coll_tuned_allgatherv_algorithm sparbit`` to your ``mpirun``
      command.  Thanks to Wilton Jaciel Loch and Guilherme Koslovski
      for their contribution.
    - Updated the usage of ``.gitmodules`` to use relative paths from
      absolute paths. This allows the submodule cloning to use the
      same protocol as Open MPI cloning. Thanks to Felix Uhl for the
      contribution.
