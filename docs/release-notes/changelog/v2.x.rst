Open MPI v2.x series
====================

This file contains all the NEWS updates for all the Open MPI v2.x
series, in reverse chronological order.

Open MPI v2.1.x series
----------------------

Open MPI version 2.1.5
^^^^^^^^^^^^^^^^^^^^^^
:Date: August 2018

- A subtle race condition bug was discovered in the "vader" BTL
  (shared memory communications) that, in rare instances, can cause
  MPI processes to crash or incorrectly classify (or effectively drop)
  an MPI message sent via shared memory.  If you are using the "ob1"
  PML with "vader" for shared memory communication (note that vader is
  the default for shared memory communication with ob1), you need to
  upgrade to v2.1.5 to fix this issue.  You may also upgrade to the
  following versions to fix this issue:

  - Open MPI v3.0.1 (released March, 2018) or later in the v3.0.x
    series
  - Open MPI v3.1.2 (expected end of August, 2018) or later

- A link issue was fixed when the UCX library was not located in the
  linker-default search paths.

Open MPI version 2.1.4
^^^^^^^^^^^^^^^^^^^^^^
:Date: August, 2018

- Disable the POWER 7/BE block in configure.  Note that POWER 7/BE is
  still not a supported platform, but it is no longer automatically
  disabled.  See
  https://github.com/open-mpi/ompi/issues/4349#issuecomment-374970982
  for more information.
- Fix bug with request-based one-sided MPI operations when using the
  "rdma" component.
- Fix issue with large data structure in the TCP BTL causing problems
  in some environments.  Thanks to @lgarithm for reporting the issue.
- Minor Cygwin build fixes.
- Minor fixes for the openib BTL:
  1. Support for the QLogic RoCE HCA
  2. Support for the Boradcom Cumulus RoCE HCA
  3. Enable support for HDR link speeds
- Fix MPI_FINALIZED hang if invoked from an attribute destructor
  during the MPI_COMM_SELF destruction in MPI_FINALIZE.  Thanks to
  @AndrewGaspar for reporting the issue.
- Java fixes:

  - Modernize Java framework detection, especially on OS X/MacOS.
    Thanks to Bryce Glover for reporting and submitting the fixes.
  - Prefer "javac -h" to "javah" to support newer Java frameworks.

- Fortran fixes:

  - Use conformant dummy parameter names for Fortran bindings.  Thanks
    to Themos Tsikas for reporting and submitting the fixes.
  - Build the MPI_SIZEOF() interfaces in the "TKR"-style "mpi" module
    whenever possible.  Thanks to Themos Tsikas for reporting the
    issue.
  - Fix array of argv handling for the Fortran bindings of
    MPI_COMM_SPAWN_MULTIPLE (and its associated man page).
  - Make NAG Fortran compiler support more robust in configure.

- Disable the "pt2pt" one-sided MPI component when MPI_THREAD_MULTIPLE
  is used.  This component is simply not safe in MPI_THREAD_MULTIPLE
  scenarios, and will not be fixed in the v2.1.x series.
- Make the "external" hwloc component fail gracefully if it is tries
  to use an hwloc v2.x.y installation.  hwloc v2.x.y will not be
  supported in the Open MPI v2.1.x series.
- Fix "vader" shared memory support for messages larger than 2GB.
  Thanks to Heiko Bauke for the bug report.
- Configure fixes for external PMI directory detection.  Thanks to
  Davide Vanzo for the report.


Open MPI version 2.1.3
^^^^^^^^^^^^^^^^^^^^^^
:Date: March, 2018

- Update internal PMIx version to 1.2.5.
- Fix a problem with ompi_info reporting using param option.
  Thanks to Alexander Pozdneev for reporting.
- Correct PMPI_Aint_{add|diff} to be functions (not subroutines)
  in the Fortran mpi_f08 module.
- Fix a problem when doing MPI I/O using data types with large
  extents in conjunction with MPI_TYPE_CREATE_SUBARRAY.  Thanks to
  Christopher Brady for reporting.
- Fix a problem when opening many files using MPI_FILE_OPEN.
  Thanks to William Dawson for reporting.
- Fix a problem with debuggers failing to attach to a running job.
  Thanks to Dirk Schubert for reporting.
- Fix a problem when using madvise and the OpenIB BTL.  Thanks to
  Timo Bingmann for reporting.
- Fix a problem in the Vader BTL that resulted in failures of
  IMB under certain circumstances.  Thanks to Nicolas Morey-
  Chaisemartin for reporting.
- Fix a problem preventing Open MPI from working under Cygwin.
  Thanks to Marco Atzeri for reporting.
- Reduce some verbosity being emitted by the USNIC BTL under certain
  circumstances.  Thanks to Peter Forai for reporting.
- Fix a problem with misdirection of SIGKILL.  Thanks to Michael Fern
  for reporting.
- Replace use of posix_memalign with malloc for small allocations.  Thanks
  to Ben Menaude for reporting.
- Fix a problem with Open MPI's out of band TCP network for file descriptors
  greater than 32767.  Thanks to Wojtek Wasko for reporting and fixing.
- Plug a memory leak in MPI_Mem_free().  Thanks to Philip Blakely for reporting.


Open MPI version 2.1.2
^^^^^^^^^^^^^^^^^^^^^^
:Date: September, 2017

- Update internal PMIx version to 1.2.3.
- Fix some problems when using the NAG Fortran compiler to build Open MPI
  and when using the compiler wrappers.  Thanks to Neil Carlson for reporting.
- Fix a compilation problem with the SM BTL.  Thanks to Paul Hargrove for
  reporting.
- Fix a problem with MPI_IALLTOALLW when using zero-length messages.
  Thanks to Dahai Guo for reporting.
- Fix a problem with C11 generic type interface for SHMEM_G.  Thanks
  to Nick Park for reporting.
- Switch to using the lustreapi.h include file when building Open MPI
  with Lustre support.
- Fix a problem in the OB1 PML that led to hangs with OSU collective tests.
- Fix a progression issue with MPI_WIN_FLUSH_LOCAL.  Thanks to
  Joseph Schuchart for reporting.
- Fix an issue with recent versions of PBSPro requiring libcrypto.
  Thanks to Petr Hanousek for reporting.
- Fix a problem when using MPI_ANY_SOURCE with MPI_SENDRECV.
- Fix an issue that prevented signals from being propagated to ORTE
  daemons.
- Ensure that signals are forwarded from ORTE daemons to all processes
  in the process group created by the daemons.  Thanks to Ted Sussman
  for reporting.
- Fix a problem with launching a job under a debugger. Thanks to
  Greg Lee for reporting.
- Fix a problem with Open MPI native I/O MPI_FILE_OPEN when using
  a communicator having an associated topology.  Thanks to
  Wei-keng Liao for reporting.
- Fix an issue when using MPI_ACCUMULATE with derived datatypes.
- Fix a problem with Fortran bindings that led to compilation errors
  for user defined reduction operations.  Thanks to Nathan Weeks for
  reporting.
- Fix ROMIO issues with large writes/reads when using NFS file systems.
- Fix definition of Fortran MPI_ARGV_NULL and MPI_ARGVS_NULL.
- Enable use of the head node of a SLURM allocation on Cray XC systems.
- Fix a problem with synchronous sends when using the UCX PML.
- Use default socket buffer size to improve TCP BTL performance.
- Add a mca parameter ras_base_launch_orted_on_hn to allow for launching
  MPI processes on the same node where mpirun is executing using a separate
  orte daemon, rather than the mpirun process.   This may be useful to set to
  true when using SLURM, as it improves interoperability with SLURM's signal
  propagation tools.  By default it is set to false, except for Cray XC systems.
- Fix ``--without-lsf`` when lsf is installed in the default search path.
- Remove support for big endian PowerPC.
- Remove support for XL compilers older than v13.1
- Remove IB XRC support from the OpenIB BTL due to loss of maintainer.


Open MPI version 2.1.1
^^^^^^^^^^^^^^^^^^^^^^
:Date: April, 2017

- Fix a problem with one of Open MPI's fifo data structures which led to
  hangs in a make check test.  Thanks to Nicolas Morey-Chaisemartin for
  reporting.
- Add missing MPI_AINT_ADD/MPI_AINT_DIFF function definitions to mpif.h.
  Thanks to Aboorva Devarajan for reporting.
- Fix the error return from MPI_WIN_LOCK when rank argument is invalid.
  Thanks to Jeff Hammond for reporting and fixing this issue.
- Fix a problem with mpirun/orterun when started under a debugger. Thanks
  to Gregory Leff for reporting.
- Add configury option to disable use of CMA by the vader BTL.  Thanks
  to Sascha Hunold for reporting.
- Add configury check for MPI_DOUBLE_COMPLEX datatype support.
  Thanks to Alexander Klein for reporting.
- Fix memory allocated by MPI_WIN_ALLOCATE_SHARED to
  be 64 bit aligned.  Thanks to Joseph Schuchart for
  reporting.
- Update MPI_WTICK man page to reflect possibly higher
  resolution than 10e-6.  Thanks to Mark Dixon for
  reporting
- Add missing MPI_T_PVAR_SESSION_NULL definition to mpi.h
  include file.  Thanks to Omri Mor for this contribution.
- Enhance the Open MPI spec file to install modulefile in ``/opt``
  if installed in a non-default location.  Thanks to Kevin
  Buckley for reporting and supplying a fix.
- Fix a problem with conflicting PMI symbols when linking statically.
  Thanks to Kilian Cavalotti for reporting.

.. note:: Known issues (to be addressed in v2.1.2):

   - See the list of fixes slated for v2.1.2 here:
     https://github.com/open-mpi/ompi/milestone/28?closed=1


Open MPI version 2.1.0
^^^^^^^^^^^^^^^^^^^^^^
:Date: March, 2017

.. important:: Major new features:

   - The main focus of the Open MPI v2.1.0 release was to update to PMIx
     v1.2.1.  When using PMIx (e.g., via mpirun-based launches, or via
     direct launches with recent versions of popular resource managers),
     launch time scalability is improved, and the run time memory
     footprint is greatly decreased when launching large numbers of MPI /
     OpenSHMEM processes.
   - Update OpenSHMEM API conformance to v1.3.
   - The usnic BTL now supports MPI_THREAD_MULTIPLE.
   - General/overall performance improvements to MPI_THREAD_MULTIPLE.
   - Add a summary message at the bottom of configure that tells you many
     of the configuration options specified and/or discovered by Open MPI.

.. attention:: Removed legacy support:

   - The ptmalloc2 hooks have been removed from the Open MPI code base.
     This is not really a user-noticable change; it is only mentioned
     here because there was much rejoycing in the Open MPI developer
     community.

- New MCA parameters:

  - **iof_base_redirect_app_stderr_to_stdout**: as its name implies, it
    combines MPI / OpenSHMEM applications' stderr into its stdout
    stream.
  - **opal_event_include**: allow the user to specify which FD selection
    mechanism is used by the underlying event engine.
  - opal_stacktrace_output: indicate where stacktraces should be sent
    upon MPI / OpenSHMEM process crashes (``none``, ``stdout``,
    ``stderr``, ``file:filename``).
  - **orte_timeout_for_stack_trace**: number of seconds to wait for stack
    traces to be reported (or ``<=0`` to wait forever).
  - **mtl_ofi_control_prog_type**/**mtl_ofi_data_prog_type**: specify
    libfabric progress model to be used for control and data.

- Fix MPI_WTICK regression where the time reported may be inaccurate
  on systems with processor frequency scalaing enabled.
- Fix regression that lowered the memory maximum message bandwidth for
  large messages on some BTL network transports, such as openib, sm,
  and vader.
- Fix a name collision in the shared file pointer MPI IO file locking
  scheme.  Thanks to Nicolas Joly for reporting the issue.
- Fix datatype extent/offset errors in MPI_PUT and MPI_RACCUMULATE
  when using the Portals 4 one-sided component.
- Add support for non-contiguous datatypes to the Portals 4 one-sided
  component.
- Various updates for the UCX PML.
- Updates to the following man pages:

  - mpirun(1)
  - MPI_COMM_CONNECT(3)
  - MPI_WIN_GET_NAME(3). Thanks to Nicolas Joly for reporting the
    typo.
  - MPI_INFO_GET_[NKEYS|NTHKEY](3). Thanks to Nicolas Joly for
    reporting the typo.

- Fixed a problem in the TCP BTL when using MPI_THREAD_MULTIPLE.
  Thanks to Evgueni Petrov for reporting.
- Fixed external32 representation in the romio314 module.  Note that
  for now, external32 representation is not correctly supported by the
  ompio module.  Thanks to Thomas Gastine for bringing this to our
  attention.
- Add note how to disable a warning message about when a high-speed
  MPI transport is not found.  Thanks to Susan Schwarz for reporting
  the issue.
- Ensure that sending SIGINT when using the rsh/ssh launcher does not
  orphan children nodes in the launch tree.
- Fix the help message when showing deprecated MCA param names to show
  the correct (i.e., deprecated) name.
- Enable support for the openib BTL to use multiple different
  InfiniBand subnets.
- Fix a minor error in MPI_AINT_DIFF.
- Fix bugs with MPI_IN_PLACE handling in:

  - MPI_ALLGATHER[V]
  - MPI_[I][GATHER|SCATTER][V]
  - MPI_IREDUCE[_SCATTER]
  - Thanks to all the users who helped diagnose these issues.

- Allow qrsh to tree spawn (if the back-end system supports it).
- Fix MPI_T_PVAR_GET_INDEX to return the correct index.
- Correctly position the shared file pointer in append mode in the
  OMPIO component.
- Add some deprecated names into shmem.h for backwards compatibility
  with legacy codes.
- Fix MPI_MODE_NOCHECK support.
- Fix a regression in PowerPC atomics support.  Thanks to Orion
  Poplawski for reporting the issue.
- Fixes for assembly code with aggressively-optimized compilers on
  x86_64/AMD64 platforms.
- Fix one more place where configure was mangling custom CFLAGS.
  Thanks to Phil Tooley (@Telemin) for reporting the issue.
- Better handle builds with external installations of hwloc.
- Fixed a hang with MPI_PUT and MPI_WIN_LOCK_ALL.
- Fixed a bug when using MPI_GET on non-contiguous datatypes and
  MPI_LOCK/MPI_UNLOCK.
- Fixed a bug when using POST/START/COMPLETE/WAIT after a fence.
- Fix configure portability by cleaning up a few uses of "==" with
  "test".  Thanks to Kevin Buckley for pointing out the issue.
- Fix bug when using darrays with lib and extent of darray datatypes.
- Updates to make Open MPI binary builds more bit-for-bit
  reproducable.  Thanks to Alastair McKinstry for the suggestion.
- Fix issues regarding persistent request handling.
- Ensure that shmemx.h is a standalone OpenSHMEM header file.  Thanks
  to Nick Park (@nspark) for the report.
- Ensure that we always send SIGTERM prior to SIGKILL.  Thanks to Noel
  Rycroft for the report.
- Added ConnectX-5 and Chelsio T6 device defaults for the openib BTL.
- OpenSHMEM no longer supports MXM less than v2.0.
- Plug a memory leak in ompi_osc_sm_free.  Thanks to Joseph Schuchart
  for the report.
- The "self" BTL now uses less memory.
- The vader BTL is now more efficient in terms of memory usage when
  using XPMEM.
- Removed the ``--enable-openib-failover`` configure option.  This is not
  considered backwards-incompatible because this option was stale and
  had long-since stopped working, anyway.
- Allow jobs launched under Cray aprun to use hyperthreads if
  opal_hwloc_base_hwthreads_as_cpus MCA parameter is set.
- Add support for 32-bit and floating point Cray Aries atomic
  operations.
- Add support for network AMOs for MPI_ACCUMULATE, MPI_FETCH_AND_OP,
  and MPI_COMPARE_AND_SWAP if the "ompi_single_intrinsic" info key is
  set on the window or the "acc_single_intrinsic" MCA param is set.
- Automatically disqualify RDMA CM support in the openib BTL if
  MPI_THREAD_MULTIPLE is used.
- Make configure smarter/better about auto-detecting Linux CMA
  support.
- Improve the scalability of MPI_COMM_SPLIT_TYPE.
- Fix the mixing of C99 and C++ header files with the MPI C++
  bindings.  Thanks to Alastair McKinstry for the bug report.
- Add support for ARM v8.
- Several MCA parameters now directly support MPI_T enumerator
  semantics (i.e., they accept a limited set of values |mdash| e.g., MCA
  parameters that accept boolean values).
- Added ``--with-libmpi-name=<STRING>`` configure option for vendor
  releases of Open MPI.  See the README for more detail.
- Fix a problem with Open MPI's internal memory checker.  Thanks to Yvan
  Fournier for reporting.
- Fix a multi-threaded issue with MPI_WAIT.  Thanks to Pascal Deveze for
  reporting.

.. note:: Known issues (to be addressed in v2.1.1):

   - See the list of fixes slated for v2.1.1 here:
     https://github.com/open-mpi/ompi/milestone/26?closed=1


Open MPI v2.0.x series
----------------------

Open MPI version 2.0.4
^^^^^^^^^^^^^^^^^^^^^^
:Date: November, 2017

- Fix an issue with visibility of functions defined in the built-in PMIx.
  Thanks to Siegmar Gross for reporting this issue.
- Add configure check to prevent trying to build this release of
  Open MPI with an external hwloc 2.0 or newer release.
- Add ability to specify layered providers for OFI MTL.
- Fix a correctness issue with Open MPI's memory manager code
  that could result in corrupted message data.  Thanks to
  Valentin Petrov for reporting.
- Fix issues encountered when using newer versions of PBS Pro.
  Thanks to Petr Hanousek for reporting.
- Fix a problem with MPI_GET when using the vader BTL.  Thanks
  to Dahai Guo for reporting.
- Fix a problem when using MPI_ANY_SOURCE with MPI_SENDRECV_REPLACE.
  Thanks to Dahai Guo for reporting.
- Fix a problem using MPI_FILE_OPEN with a communicator with an
  attached cartesian topology.  Thanks to Wei-keng Liao for reporting.
- Remove IB XRC support from the OpenIB BTL due to lack of support.
- Remove support for big endian PowerPC.
- Remove support for XL compilers older than v13.1


Open MPI version 2.0.3
^^^^^^^^^^^^^^^^^^^^^^
:Date: June 2017

- Fix a problem with MPI_IALLTOALLW when zero size messages are present.
  Thanks to @mathbird for reporting.
- Add missing MPI_USER_FUNCTION definition to the mpi_f08 module.
  Thanks to Nathan Weeks for reporting this issue.
- Fix a problem with MPI_WIN_LOCK not returning an error code when
  a negative rank is supplied.  Thanks to Jeff Hammond for reporting and
  providing a fix.
- Fix a problem with make check that could lead to hangs.  Thanks to
  Nicolas Morey-Chaisemartin for reporting.
- Resolve a symbol conflict problem with PMI-1 and PMI-2 PMIx components.
  Thanks to Kilian Cavalotti for reporting this issue.
- Insure that memory allocations returned from MPI_WIN_ALLOCATE_SHARED are
  64 byte aligned.  Thanks to Joseph Schuchart for reporting this issue.
- Make use of DOUBLE_COMPLEX, if available, for Fortran bindings.  Thanks
  to Alexander Klein for reporting this issue.
- Add missing MPI_T_PVAR_SESSION_NULL definition to Open MPI mpi.h include
  file.  Thanks to Omri Mor for reporting and fixing.
- Fix a problem with use of MPI shared file pointers when accessing
  a file from independent jobs.  Thanks to Nicolas Joly for reporting
  this issue.
- Optimize zero size MPI_IALLTOALL{V,W} with MPI_IN_PLACE.  Thanks to
  Lisandro Dalcín for the report.
- Fix a ROMIO buffer overflow problem for large transfers when using NFS
  filesystems.
- Fix type of MPI_ARGV[S]_NULL which prevented it from being used
  properly with MPI_COMM_SPAWN[_MULTIPLE] in the mpi_f08 module.
- Ensure to add proper linker flags to the wrapper compilers for
  dynamic libraries on platforms that need it (e.g., RHEL 7.3 and
  later).
- Get better performance on TCP-based networks 10Gbps and higher by
  using OS defaults for buffer sizing.
- Fix a bug with ``MPI_[R][GET_]ACCUMULATE`` when using DARRAY datatypes.
- Fix handling of ``--with-lustre`` configure command line argument.
  Thanks to Prentice Bisbal and Tim Mattox for reporting the issue.
- Added MPI_AINT_ADD and MPI_AINT_DIFF declarations to mpif.h.  Thanks
  to Aboorva Devarajan (@AboorvaDevarajan) for the bug report.
- Fix a problem in the TCP BTL when Open MPI is initialized with
  MPI_THREAD_MULTIPLE support.  Thanks to Evgueni Petro for analyzing and
  reporting this issue.
- Fix yalla PML to properly handle underflow errors, and fixed a
  memory leak with blocking non-contiguous sends.
- Restored ability to run autogen.pl on official distribution tarballs
  (although this is still not recommended for most users!).
- Fix accuracy problems with MPI_WTIME on some systems by always using
  either clock_gettime(3) or gettimeofday(3).
- Fix a problem where MPI_WTICK was not returning a higher time resolution
  when available.  Thanks to Mark Dixon for reporting this issue.
- Restore SGE functionality.  Thanks to Kevin Buckley for the initial
  report.
- Fix external hwloc compilation issues, and extend support to allow
  using external hwloc installations as far back as v1.5.0.  Thanks to
  Orion Poplawski for raising the issue.
- Added latest Mellanox Connect-X and Chelsio T-6 adapter part IDs to
  the openib list of default values.
- Do a better job of cleaning up session directories (e.g., in ``/tmp``).
- Update a help message to indicate how to suppress a warning about
  no high performance networks being detected by Open MPI.  Thanks to
  Susan Schwarz for reporting this issue.
- Fix a problem with mangling of custom CFLAGS when configuring Open MPI.
  Thanks to Phil Tooley for reporting.
- Fix some minor memory leaks and remove some unused variables.
  Thanks to Joshua Gerrard for reporting.
- Fix MPI_ALLGATHERV bug with MPI_IN_PLACE.

.. note:: Known issues (to be addressed in v2.0.4):

   - See the list of fixes slated for v2.0.4 here:
     https://github.com/open-mpi/ompi/milestone/29?closed=1


Open MPI version 2.0.2
^^^^^^^^^^^^^^^^^^^^^^
:Date: 26 Jan 2017

- Fix a problem with MPI_FILE_WRITE_SHARED when using MPI_MODE_APPEND and
  Open MPI's native MPI-IO implementation.  Thanks to Nicolas Joly for
  reporting.
- Fix a typo in the MPI_WIN_GET_NAME man page.  Thanks to Nicolas Joly
  for reporting.
- Fix a race condition with ORTE's session directory setup.  Thanks to
  @tbj900 for reporting this issue.
- Fix a deadlock issue arising from Open MPI's approach to catching calls to
  munmap. Thanks to Paul Hargrove for reporting and helping to analyze this
  problem.
- Fix a problem with PPC atomics which caused make check to fail unless builtin
  atomics configure option was enabled.  Thanks to Orion Poplawski for reporting.
- Fix a problem with use of x86_64 cpuid instruction which led to segmentation
  faults when Open MPI was configured with -O3 optimization.  Thanks to Mark
  Santcroos for reporting this problem.
- Fix a problem when using built in atomics configure options on PPC platforms
  when building 32 bit applications.  Thanks to Paul Hargrove for reporting.
- Fix a problem with building Open MPI against an external hwloc installation.
  Thanks to Orion Poplawski for reporting this issue.
- Remove use of DATE in the message queue version string reported to debuggers to
  insure bit-wise reproducibility of binaries.  Thanks to Alastair McKinstry
  for help in fixing this problem.
- Fix a problem with early exit of a MPI process without calling MPI_FINALIZE
  or MPI_ABORT that could lead to job hangs.  Thanks to Christof Koehler for
  reporting.
- Fix a problem with forwarding of SIGTERM signal from mpirun to MPI processes
  in a job.  Thanks to Noel Rycroft for reporting this problem
- Plug some memory leaks in MPI_WIN_FREE discovered using Valgrind.  Thanks
  to Joseph Schuchart for reporting.
- Fix a problems  MPI_NEIGHOR_ALLTOALL when using a communicator with an empty topology
  graph.  Thanks to Daniel Ibanez for reporting.
- Fix a typo in a PMIx component help file.  Thanks to @njoly for reporting this.
- Fix a problem with Valgrind false positives when using Open MPI's internal memchecker.
  Thanks to Yvan Fournier for reporting.
- Fix a problem with MPI_FILE_DELETE returning MPI_SUCCESS when
  deleting a non-existent file. Thanks to Wei-keng Liao for reporting.
- Fix a problem with MPI_IMPROBE that could lead to hangs in subsequent MPI
  point to point or collective calls.  Thanks to Chris Pattison for reporting.
- Fix a problem when configure Open MPI for powerpc with ``--enable-mpi-cxx``
  enabled.  Thanks to Alastair McKinstry for reporting.
- Fix a problem using MPI_IALLTOALL with MPI_IN_PLACE argument.  Thanks to
  Chris Ward for reporting.
- Fix a problem using MPI_RACCUMULATE with the Portals4 transport.  Thanks to
  @PDeveze for reporting.
- Fix an issue with static linking and duplicate symbols arising from PMIx
  Slurm components.  Thanks to Limin Gu for reporting.
- Fix a problem when using MPI dynamic memory windows.  Thanks to
  Christoph Niethammer for reporting.
- Fix a problem with Open MPI's pkgconfig files.  Thanks to Alastair McKinstry
  for reporting.
- Fix a problem with MPI_IREDUCE when the same buffer is supplied for the
  send and recv buffer arguments.  Thanks to Valentin Petrov for reporting.
- Fix a problem with atomic operations on PowerPC.  Thanks to Paul
  Hargrove for reporting.

.. note:: Known issues (to be addressed in v2.0.3):

   - See the list of fixes slated for v2.0.3 here:
     https://github.com/open-mpi/ompi/milestone/23?closed=1


Open MPI version 2.0.1
^^^^^^^^^^^^^^^^^^^^^^
:Date: 2 Sep 2016

- Short message latency and message rate performance improvements for
  all transports.
- Fix shared memory performance when using RDMA-capable networks.
  Thanks to Tetsuya Mishima and Christoph Niethammer for reporting.
- Fix bandwith performance degredation in the yalla (MXM) PML.  Thanks
  to Andreas Kempf for reporting the issue.
- Fix OpenSHMEM crash when running on non-Mellanox MXM-based networks.
  Thanks to Debendra Das for reporting the issue.
- Fix a crash occuring after repeated calls to MPI_FILE_SET_VIEW with
  predefined datatypes.  Thanks to Eric Chamberland and Matthew
  Knepley for reporting and helping chase down this issue.
- Fix stdin propagation to MPI processes.  Thanks to Jingchao Zhang
  for reporting the issue.
- Fix various runtime and portability issues by updating the PMIx
  internal component to v1.1.5.
- Fix process startup failures on Intel MIC platforms due to very
  large entries in ``/proc/mounts``.
- Fix a problem with use of relative path for specifing executables to
  mpirun / oshrun.  Thanks to David Schneider for reporting.
- Various improvements when running over portals-based networks.
- Fix thread-based race conditions with GNI-based networks.
- Fix a problem with MPI_FILE_CLOSE and MPI_FILE_SET_SIZE.  Thanks
  to Cihan Altinay for reporting.
- Remove all use of rand(3) from within Open MPI so as not to perturb
  applications use of it.  Thanks to Matias Cabral and Noel Rycroft
  for reporting.
- Fix crash in MPI_COMM_SPAWN.
- Fix types for MPI_UNWEIGHTED and MPI_WEIGHTS_EMPTY.  Thanks to
  Lisandro Dalcín for reporting.
- Correctly report the name of MPI_INTEGER16.
- Add some missing MPI constants to the Fortran bindings.
- Fixed compile error when configuring Open MPI with ``--enable-timing``.
- Correctly set the shared library version of libompitrace.so.  Thanks
  to Alastair McKinstry for reporting.
- Fix errors in the MPI_RPUT, MPI_RGET, MPI_RACCUMULATE, and
  MPI_RGET_ACCUMULATE Fortran bindings.  Thanks to Alfio Lazzaro and
  Joost VandeVondele for tracking this down.
- Fix problems with use of derived datatypes in non-blocking
  collectives.  Thanks to Yuki Matsumoto for reporting.
- Fix problems with OpenSHMEM header files when using CMake.  Thanks to
  Paul Kapinos for reporting the issue.
- Fix problem with use use of non-zero lower bound datatypes in
  collectives.  Thanks to Hristo Iliev for reporting.
- Fix a problem with memory allocation within MPI_GROUP_INTERSECTION.
  Thanks to Lisandro Dalcín for reporting.
- Fix an issue with MPI_ALLGATHER for communicators that don't consist
  of two ranks.  Thanks to David Love for reporting.
- Various fixes for collectives when used with esoteric MPI datatypes.
- Fixed corner cases of handling DARRAY and HINDEXED_BLOCK datatypes.
- Fix a problem with filesystem type check for OpenBSD.
  Thanks to Paul Hargrove for reporting.
- Fix some debug input within Open MPI internal functions.  Thanks to
  Durga Choudhury for reporting.
- Fix a typo in a configury help message.  Thanks to Paul Hargrove for
  reporting.
- Correctly support MPI_IN_PLACE in MPI_[I]ALLTOALL[V|W] and
  MPI_[I]EXSCAN.
- Fix alignment issues on SPARC platforms.

.. note:: Known issues (to be addressed in v2.0.2):

   - See the list of fixes slated for v2.0.2 here:
     https://github.com/open-mpi/ompi/milestone/20?closed=1, and
     https://github.com/open-mpi/ompi-release/milestone/19?closed=1
     (note that the "ompi-release" Github repo will be folded/absorbed
     into the "ompi" Github repo at some point in the future)


Open MPI version 2.0.0
^^^^^^^^^^^^^^^^^^^^^^
:Date: 12 Jul 2016

.. attention:: Open MPI is now fully MPI-3.1 compliant

.. important:: Major new features:

   - Many enhancements to MPI RMA.  Open MPI now maps MPI RMA operations
     on to native RMA operations for those networks which support this
     capability.
   - Greatly improved support for MPI_THREAD_MULTIPLE (when configured
     with ``--enable-mpi-thread-multiple``).
   - Enhancements to reduce the memory footprint for jobs at scale.  A
     new MCA parameter, "mpi_add_procs_cutoff", is available to set the
     threshold for using this feature.
   - Completely revamped support for memory registration hooks when using
     OS-bypass network transports.
   - Significant OMPIO performance improvements and many bug fixes.
   - Add support for PMIx - Process Management Interface for Exascale.
     Version 1.1.2 of PMIx is included internally in this release.
   - Add support for PLFS file systems in Open MPI I/O.
   - Add support for UCX transport.
   - Simplify build process for Cray XC systems.  Add support for
     using native SLURM.
   - Add a ``--tune`` mpirun command line option to simplify setting many
     environment variables and MCA parameters.
   - Add a new MCA parameter "orte_default_dash_host" to offer an analogue
     to the existing "orte_default_hostfile" MCA parameter.
   - Add the ability to specify the number of desired slots in the mpirun
     ``--host`` option.

.. note:: Known issues (to be addressed in v2.0.1):

   - See the list of fixes slated for v2.0.1 here:
     https://github.com/open-mpi/ompi/milestone/16?closed=1, and
     https://github.com/open-mpi/ompi-release/milestone/16?closed=1
     (note that the "ompi-release" Github repo will be folded/absorbed
     into the "ompi" Github repo at some point in the future)

   - ompi-release#986: Fix data size counter for large ops with fcoll/static
   - ompi-release#987: Fix OMPIO performance on Lustre
   - ompi-release#1013: Fix potential inconsistency in btl/openib default settings
   - ompi-release#1014: Do not return MPI_ERR_PENDING from collectives
   - ompi-release#1056: Remove dead profile code from oshmem
   - ompi-release#1081: Fix MPI_IN_PLACE checking for IALLTOALL{V|W}
   - ompi-release#1081: Fix memchecker in MPI_IALLTOALLW
   - ompi-release#1081: Support MPI_IN_PLACE in MPI_(I)ALLTOALLW and MPI_(I)EXSCAN
   - ompi-release#1107: Allow future PMIx support for RM spawn limits
   - ompi-release#1108: Fix sparse group process reference counting
   - ompi-release#1109: If specified to be oversubcribed, disable binding
   - ompi-release#1122: Allow NULL arrays for empty datatypes
   - ompi-release#1123: Fix signed vs. unsigned compiler warnings
   - ompi-release#1123: Make max hostname length uniform across code base
   - ompi-release#1127: Fix MPI_Compare_and_swap
   - ompi-release#1127: Fix MPI_Win_lock when used with MPI_Win_fence
   - ompi-release#1132: Fix typo in help message for ``--enable-mca-no-build``
   - ompi-release#1154: Ensure pairwise coll algorithms disqualify themselves properly
   - ompi-release#1165: Fix typos in debugging/verbose message output
   - ompi-release#1178: Fix ROMIO filesystem check on OpenBSD 5.7
   - ompi-release#1197: Fix Fortran pthread configure check
   - ompi-release#1205: Allow using external PMIx 1.1.4 and 2.0
   - ompi-release#1215: Fix configure to support the NAG Fortran compiler
   - ompi-release#1220: Fix combiner args for MPI_HINDEXED_BLOCK
   - ompi-release#1225: Fix combiner args for MPI_DARRAY
   - ompi-release#1226: Disable old memory hooks with recent gcc versions
   - ompi-release#1231: Fix new "patcher" support for some XLC platforms
   - ompi-release#1244: Fix Java error handling
   - ompi-release#1250: Ensure TCP is not selected for RDMA operations
   - ompi-release#1252: Fix verbose output in coll selection
   - ompi-release#1253: Set a default name for user-defined MPI_Op
   - ompi-release#1254: Add count==0 checks in some non-blocking colls
   - ompi-release#1258: Fix ``make distclean`` when using external pmix/hwloc/libevent
   - ompi-release#1260: Clean up/uniform mca/coll/base memory management
   - ompi-release#1261: Remove "patcher" warning message for static builds
   - ompi-release#1263: Fix IO MPI_Request for 0-size read/write
   - ompi-release#1264: Add blocking fence for SLURM operations

- In environments where mpirun cannot automatically determine the
  number of slots available (e.g., when using a hostfile that does not
  specify "slots", or when using ``--host`` without specifying a ":N"
  suffix to hostnames), mpirun now requires the use of "-np N" to
  specify how many MPI processes to launch.
- The MPI C++ bindings (which were removed from the MPI standard in
  v3.0) are no longer built by default and will be removed in some
  future version of Open MPI.  Use the ``--enable-mpi-cxx-bindings``
  configure option to build the deprecated/removed MPI C++ bindings.
- ompi_info now shows all components, even if they do not have MCA
  parameters.  The prettyprint output now separates groups with a
  dashed line.
- OMPIO is now the default implementation of parallel I/O, with the
  exception for Lustre parallel filesystems (where ROMIO is still the
  default).  The default selection of OMPI vs. ROMIO can be controlled
  via the "--mca io ompi|romio" command line switch to mpirun.
- Per Open MPI's versioning scheme (see the README), increasing the
  major version number to 2 indicates that this version is not
  ABI-compatible with prior versions of Open MPI.  You will need to
  recompile MPI and OpenSHMEM applications to work with this version
  of Open MPI.
- Removed checkpoint/restart code due to loss of maintainer. :-(
- Change the behavior for handling certain signals when using PSM and
  PSM2 libraries.  Previously, the PSM and PSM2 libraries would trap
  certain signals in order to generate tracebacks.  The mechanism was
  found to cause issues with Open MPI's own error reporting mechanism.
  If not already set, Open MPI now sets the IPATH_NO_BACKTRACE and
  HFI_NO_BACKTRACE environment variables to disable PSM/PSM2's
  handling these signals.

.. attention:: Removed legacy support:

   - Removed support for OS X Leopard.
   - Removed support for Cray XT systems.
   - Removed VampirTrace.
   - Removed support for Myrinet/MX.
   - Removed legacy collective module:ML.
   - Removed support for Alpha processors.
   - Removed ``--enable-mpi-profiling`` configure option.

- Updated internal/embedded copies of third-party software:

  - Update the internal copy of ROMIO to that which shipped in MPICH
    3.1.4.
  - Update internal copy of libevent to v2.0.22.
  - Update internal copy of hwloc to v1.11.2.

- Notable new MCA parameters:

  - opal_progress_lp_call_ration: Control how often low-priority
    callbacks are made during Open MPI's main progress loop.
  - opal_common_verbs_want_fork_support: This replaces the
    btl_openib_want_fork_support parameter.

- Add ``--with-platform-patches-dir`` configure option.
- Add ``--with-pmi-libdir`` configure option for environments that install
  PMI libs in a non-default location.
- Various configure-related compatibility updates for newer versions
  of libibverbs and OFED.
- Numerous fixes/improvements to orte-dvm.  Special thanks to Mark
  Santcroos for his help.
- Fix a problem with timer code on ia32 platforms.  Thanks to
  Paul Hargrove for reporting this and providing a patch.
- Fix a problem with use of a 64 bit atomic counter.  Thanks to
  Paul Hargrove for reporting.
- Fix a problem with singleton job launching.  Thanks to Lisandro
  Dalcín for reporting.
- Fix a problem with use of MPI_UNDEFINED with MPI_COMM_SPLIT_TYPE.
  Thanks to Lisandro Dalcín for reporting.
- Silence a compiler warning in PSM MTL.  Thanks to Adrian Reber for
  reporting this.
- Properly detect Intel TrueScale and OmniPath devices in the ACTIVE
  state.  Thanks to Durga Choudhury for reporting the issue.
- Fix detection and use of Solaris Studio 12.5 (beta) compilers.
  Thanks to Paul Hargrove for reporting and debugging.
- Fix various small memory leaks.
- Allow NULL arrays when creating empty MPI datatypes.
- Replace use of alloca with malloc for certain datatype creation
  functions.  Thanks to Bogdan Sataric for reporting this.
- Fix use of MPI_LB and MPI_UB in creation of of certain MPI datatypes.
  Thanks to Gus Correa for helping to fix this.
- Implement a workaround for a GNU Libtool problem.  Thanks to Eric
  Schnetter for reporting and fixing.
- Improve hcoll library detection in configure.  Thanks to David
  Shrader and Åke Sandgren for reporting this.
- Miscellaneous minor bug fixes in the hcoll component.
- Miscellaneous minor bug fixes in the ugni component.
- Fix problems with XRC detection in OFED 3.12 and older releases.
  Thanks to Paul Hargrove for his analysis of this problem.
- Update (non-standard/experimental) Java MPI interfaces to support
  MPI-3.1 functionality.
- Fix an issue with MCA parameters for Java bindings.  Thanks to
  Takahiro Kawashima and Siegmar Gross for reporting this issue.
- Fix a problem when using persistent requests in the Java bindings.
  Thanks to Nate Chambers for reporting.
- Fix problem with Java bindings on OX X 10.11.  Thanks to Alexander
  Daryin for reporting this issue.
- Fix a performance problem for large messages for Cray XC systems.
  Thanks to Jerome Vienne for reporting this.
- Fix an issue with MPI_WIN_LOCK_ALL.  Thanks to Thomas Jahns for
  reporting.
- Fix an issue with passing a parameter to configure multiple times.
  Thanks to QuesarVII for reporting and supplying a fix.
- Add support for ALPS resource allocation system on Cray CLE 5.2 and
  later.  Thanks to Mark Santcroos.
- Corrections to the HACKING file.  Thanks to Maximilien Levesque.
- Fix an issue with user supplied reduction operator functions.
  Thanks to Rupert Nash for reporting this.
- Fix an issue with an internal list management function.  Thanks to
  Adrian Reber for reporting this.
- Fix a problem with MPI-RMA PSCW epochs.  Thanks to Berk Hess for
  reporting this.
- Fix a problem in neighborhood collectives.  Thanks to Lisandro
  Dalcín for reporting.
- Fix MPI_IREDUCE_SCATTER_BLOCK for a one-process communicator. Thanks
  to Lisandro Dalcín for reporting.
- Add (Open MPI-specific) additional flavors to MPI_COMM_SPLIT_TYPE.
  See MPI_Comm_split_type(3) for details.  Thanks to Nick Andersen for
  supplying this enhancement.
- Improve closing of file descriptors during the job launch phase.
  Thanks to Piotr Lesnicki for reporting and providing this
  enhancement.
- Fix a problem in MPI_GET_ACCUMULATE and MPI_RGET_ACCUMULATE when
  using Portals4.  Thanks to Nicolas Chevalier for reporting.
- Use correct include file for lstat prototype in ROMIO. Thanks to
  William Throwe for finding and providing a fix.
- Add missing Fortran bindings for MPI_WIN_ALLOCATE.  Thanks to Christoph
  Niethammer for reporting and fixing.
- Fortran related fixes to handle Intel 2016 compiler.  Thanks to
  Fabrice Roy for reporting this.
- Fix a Fortran linkage issue.  Thanks to Macro Atzeri for finding and
  suggesting a fix.
- Fix problem with using BIND(C) for Fortran bindings with logical
  parameters.  Thanks to Paul Romano for reporting.
- Fix an issue with use of DL-related macros in opal library.  Thanks to
  Scott Atchley for finding this.
- Fix an issue with parsing mpirun command line options which contain
  colons.  Thanks to Lev Given for reporting.
- Fix a problem with Open MPI's package configury files.  Thanks to
  Christoph Junghans for reporting.
- Fix a typo in the MPI_INTERCOMM_MERGE man page.  Thanks To Harald
  Servat for reporting and correcting.
- Update man pages for non-blocking sends per MPI 3.1 standard.
  Thanks to Alexander Pozdneev for reporting.
- Fix problem when compiling against PVFS2.  Thanks to Dave Love for
  reporting.
- Fix problems with MPI_NEIGHBOR_ALLTOALL{V,W}.  Thanks to Willem
  Vermin for reporting this issue.
- Fix various compilation problems on Cygwin.  Thanks to Marco Atzeri
  for supplying these fixes.
- Fix problem with resizing of subarray and darray data types.  Thanks
  to Keith Bennett and Dan Garmann for reporting.
- Fix a problem with MPI_COMBINER_RESIZED.  Thanks to James Ramsey for
  the report.
- Fix an hwloc binding issue.  Thanks to Ben Menadue for reporting.
- Fix a problem with the shared memory (sm) BTL.  Thanks to Peter Wind
  for the report.
- Fixes for heterogeneous support. Thanks to Siegmar Gross for reporting.
- Fix a problem with memchecker.  Thanks to Clinton Simpson for reporting.
- Fix a problem with MPI_UNWEIGHTED in topology functions.  Thanks to
  Jun Kudo for reporting.
- Fix problem with a MCA parameter base filesystem types.  Thanks to
  Siegmar Gross for reporting.
- Fix a problem with some windows info argument types.  Thanks to
  Alastair McKinstry for reporting.
