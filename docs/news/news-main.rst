Master updates (not on release branches yet)
============================================

This file generally contains all the updates for Open MPI that have
not yet appeared on a release branch.  It reflects active development,
and is therefore a "loose" listing of features and changes.  It is not
considered definitive.

Open MPI version 5.0.0rc2
-------------------------
:Date: 10 Oct 2021

.. admonition:: MPIR API has been removed
   :class: warning

   As was announced in summer 2017, Open MPI has removed support of
   MPIR-based tools beginning with the release of Open MPI v5.0.0.

   The new PRRTE based runtime environment supports PMIx-tools API
   instead of the legacy MPIR API for debugging parallel jobs.

   see https://github.com/openpmix/mpir-to-pmix-guide for more
   information


.. admonition:: zlib is suggested for better user experience
   :class: note

   PMIx will optionally use zlib to compress large data streams.
   This may result in shorter-than-normal startup times and
   smaller memory footprints.  It is recommended to install zlib
   and zlib-devel for a better user experience.

- ORTE, the underlying OMPI launcher has been removed, and replaced
  with PRTE.
- Reworked how Open MPI integrates with 3rd party packages.
  The decision was made to stop building 3rd-party packages
  such as Libevent, HWLOC, PMIx, and PRRTE as MCA components
  and instead 1) start relying on external libraries whenever
  possible and 2) Open MPI builds the 3rd party libraries (if needed)
  as independent libraries, rather than linked into libopen-pal.
- Update to use PMIx v4.1.1rc2
- Update to use PRRTE v2.0.1rc2
- Change the default component build behavior to prefer building
  components as part of libmpi.so instead of individual DSOs.
- Remove pml/yalla, mxm, mtl/psm, and ikrit components.
- Remove all vestiges of the C/R support.
- Various ROMIO v3.4.1 updates.
- Use Pandoc to generate manpages
- 32 bit atomics are now only supported via C11 compliant compilers.
- Explicitly disable support for GNU gcc < v4.8.1 (note: the default
  gcc compiler that is included in RHEL 7 is v4.8.5).
- Do not build Open SHMEM layer when there are no SPMLs available.
  Currently, this means the Open SHMEM layer will only build if
  the UCX library is found.
- Fix rank-by algorithms to properly rank by object and span.
- Updated the ``-mca pml`` option to only accept one pml, not a list.
- vprotocol/pessimist: Updated to support ``MPI_THREAD_MULLTIPLE``.
- btl/tcp: Updated to use reachability and graph solving for global
  interface matching. This has been shown to improve ``MPI_Init()``
  performance under btl/tcp.
- fs/ime: Fixed compilation errors due to missing header inclusion
  Thanks to Sylvain Didelot <sdidelot@ddn.com> for finding
  and fixing this issue.
- Fixed bug where MPI_Init_thread can give wrong error messages by
  delaying error reporting until all infrastructure is running.
- Atomics support removed: S390/s390x, Sparc v9, ARMv4 and ARMv5 CMA
  support.
- ``autogen.pl`` now supports a ``-j`` option to run multi-threaded.
  Users can also use environment variable ``AUTOMAKE_JOBS``.
- PMI support has been removed for Open MPI apps.
- Legacy btl/sm has been removed, and replaced with btl/vader, which
  was renamed to btl/sm.
- Update btl/sm to not use CMA in user namespaces.
- C++ bindings have been removed.
- The ``--am`` and ``--amca`` options have been deprecated.
- opal/mca/threads framework added. Currently supports
  argobots, qthreads, and pthreads. See the --with-threads=x option
  in configure.
- Various ``README.md`` fixes - thanks to:
  Yixin Zhang <zhany217@wfu.edu>,
  Samuel Cho <choss@wfu.edu>,
  Robert Langfield <langrc18@wfu.edu>,
  Alex Ross <rossaj16@wfu.edu>,
  Sophia Fang <fangq18@wfu.edu>,
  mitchelltopaloglu <mitchelltopaloglu@gmail.com>,
  Evstrife <wus217@wfu.edu>, and
  Hao Tong <tongh18@gemini.deac.wfu.edu> for their
  contributions.
- osc/pt2pt: Removed. Users can use osc/rdma + btl/tcp
  for OSC support using TCP, or other providers.
- Open MPI now links -levent_core instead of -levent.
- MPI-4: Added ``ERRORS_ABORT`` infrastructure.
- common/cuda docs: Various fixes. Thanks to
  Simon Byrne <simonbyrne@gmail.com> for finding and fixing.
- osc/ucx: Add support for acc_single_intrinsic.
- Fixed ``buildrpm.sh -r`` option used for RPM options specification.
  Thanks to John K. McIver III <john.mciver.iii@gmail.com> for
  reporting and fixing.
- configure: Added support for setting the wrapper C compiler.
  Adds new option ``--with-wrapper-cc=``.
- mpi_f08: Fixed Fortran-8-byte-INTEGER vs. C-4-byte-int issue.
  Thanks to @ahaichen for reporting the bug.
- MPI-4: Added support for 'initial error handler'.
- opal/thread/tsd: Added thread-specific-data (tsd) api.
- MPI-4: Added error handling for 'unbound' errors to ``MPI_COMM_SELF``.
- Add missing ``MPI_Status`` conversion subroutines:
  ``MPI_Status_c2f08()``, ``MPI_Status_f082c()``, ``MPI_Status_f082f()``,
  ``MPI_Status_f2f08()`` and the ``PMPI_*`` related subroutines.
- patcher: Removed the Linux component.
- opal/util: Fixed typo in error string. Thanks to
  NARIBAYASHI Akira <a.naribayashi@fujitsu.com> for finding
  and fixing the bug.
- fortran/use-mpi-f08: Generate PMPI bindings from the MPI bindings.
- Converted man pages to markdown.
  Thanks to Fangcong Yin <fyin2@nd.edu> for their contribution
  to this effort.
- Fixed ompi_proc_world error string and some comments in pml/ob1.
  Thanks to Julien EMMANUEL <julien.emmanuel@inria.fr> for
  finding and fixing these issues.
- oshmem/tools/oshmem_info: Fixed Fortran keyword issue when
  compiling param.c. Thanks to Pak Lui <pak.lui@amd.com> for
  finding and fixing the bug.
- autogen.pl: Patched libtool.m4 for OSX Big Sur. Thanks to
  @fxcoudert for reporting the issue.
- Updgraded to HWLOC v2.4.0.
- Removed config/opal_check_pmi.m4.
  Thanks to Zach Osman <zosman@gmu.edu> for the contribution.
- opal/atomics: Added load-linked, store-conditional atomics for
  AArch6.
- Fixed envvar names to OMPI_MCA_orte_precondition_transports.
  Thanks to Marisa Roman <marisa.roman@cornelisnetworks.com>
  for the contribution.
- fcoll/two_phase: Removed the component. All scenerios it was
  used for has been replaced.
- btl/uct: Bumped UCX allowed version to v1.9.x.
- ULFM Fault Tolerance has been added. See ``README.FT.ULFM.md``.
- Fixed a crash during CUDA initialization.
  Thanks to Yaz Saito <yasushi.saito@gmail.com> for finding
  and fixing the bug.
- Added CUDA support to the OFI MTL.
- ompio: Added atomicity support.
- Singleton comm spawn support has been fixed.
- Autoconf v2.7 support has been updated.
- fortran: Added check for ``ISO_FORTRAN_ENV:REAL16``. Thanks to
  Jeff Hammond <jeff_hammond@acm.org> for reporting this issue.
- Changed the MCA component build style default to static.
- PowerPC atomics: Force usage of opal/ppc assembly.
- Removed C++ compiler requirement to build Open MPI.
- Fixed .la files leaking into wrapper compilers.
- Fixed bug where the cache line size was not set soon enough in
  ``MPI_Init()``.
- coll/ucc and scoll/ucc components were added.
- coll/ucc: Added support for allgather and reduce collective
  operations.
- autogen.pl: Fixed bug where it would not ignore all
  excluded components.
- Various datatype bugfixes and performance improvements
- Various pack/unpack bugfixes and performance improvements
- Fix mmap infinite recurse in memory patcher
- Fix C to Fortran error code conversions.
- osc/ucx: Fix data corruption with non-contiguous accumulates
- Update coll/tuned selection rules
- Fix non-blocking collective ops
- btl/portals4: Fix flow control
- Various oshmem:ucx bugfixes and performance improvements
- common/ofi: Disable new monitor API until libfabric 1.14.0
- Fix AVX detection with icc
- mpirun option ``--mca ompi_display_comm mpi_init``/``mpi_finalize``
  has been added. Enables a communication protocol report:
  when ``MPI_Init`` is invoked (using the ``mpi_init`` value) and/or
  when ``MPI_Finalize`` is invoked (using the ``mpi_finalize`` value).
- New algorithm for Allgather and Allgatherv added, based on the
  paper *"Sparbit: a new logarithmic-cost and data locality-aware MPI
  Allgather algorithm"*. Default algorithm selection rules are
  un-changed, to use these algorithms add:
  ``--mca coll_tuned_allgather_algorithm sparbit`` and/or
  ``--mca coll_tuned_allgatherv_algorithm sparbit``
  Thanks to: Wilton Jaciel Loch <wiltonloch wilton.loch@gmail.com>,
  and Guilherme Koslovski for their contribution.
- MPI-4: Persistent collectives have been moved to the MPI
  namespace from MPIX.
- OFI: Delay patcher initialization until needed. It will now
  be initialized only after the component is officially selected.
- MPI-4: Make ``MPI_Comm_get_info``, ``MPI_File_get_info``, and
  ``MPI_Win_get_info`` compliant to the standard.
- Portable_platform file has been updated from GASNet.
- GCC versions < 4.8.1 are no longer supported.
- coll: Fix a bug with the libnbc ``MPI_AllReduce`` ring algorithm
  when using ``MPI_IN_PLACE``.
- Updated the usage of .gitmodules to use relative paths from
  absolute paths. This allows the submodule cloning to use the same
  protocol as OMPI cloning. Thanks to Felix Uhl
  <Felix.Uhl@emea.nec.com> for the contribution.
- osc/rdma: Add local leader pid in shm file name to make it unique.
- ofi: Fix memory handler unregistration. This change fixes a
  segfault during shutdown if the common/ofi component was built
  as a dynamic object.
- osc/rdma: Add support for MPI minimum alignment key.
- memory_patcher: Add ability to detect patched memory. Thanks
  to Rich Welch <rlwelch@amazon.com> for the contribution.
- build: Improve handling of compiler version string. This
  fixes a compiler error with clang and armclang.
- Fix bug where the relocation of OMPI packages caused
  the launch to fail.
- Various improvements to ``MPI_AlltoAll`` algorithms for both
  performance and memory usage.
- coll/basic: Fix segmentation fault in ``MPI_Alltoallw`` with
  ``MPI_IN_PLACE``.
