Open MPI v3.1.x series
======================

This file contains all the NEWS updates for the Open MPI v3.1.x
series, in reverse chronological order.

Open MPI version 3.1.6
----------------------
:Date: March 2020

- Fix one-sided shared memory window configuration bug.
- Fix support for PGI'18 compiler.
- Fix issue with zero-length blockLength in ``MPI_TYPE_INDEXED``.
- Fix run-time linker issues with OMPIO on newer Linux distros.
- Fix PMIX dstore locking compilation issue.  Thanks to Marco Atzeri
  for reporting the issue.
- Allow the user to override modulefile_path in the Open MPI SRPM,
  even if ``install_in_opt`` is set to 1.
- Properly detect ConnectX-6 HCAs in the openib BTL.
- Fix segfault in the MTL/OFI initialization for large jobs.
- Fix issue to guarantee to properly release MPI one-sided lock when
  using UCX transports to avoid a deadlock.
- Fix potential deadlock when processing outstanding transfers with
  uGNI transports.
- Fix various portals4 control flow bugs.
- Fix communications ordering for alltoall and Cartesian neighborhood
  collectives.
- Fix an infinite recursion crash in the memory patcher on systems
  with glibc v2.26 or later (e.g., Ubuntu 18.04) when using certain
  OS-bypass interconnects.


Open MPI version 3.1.5
----------------------
:Date: November, 2019

- Fix OMPIO issue limiting file reads/writes to 2GB.  Thanks to
  Richard Warren for reporting the issue.
- At run time, automatically disable Linux cross-memory attach (CMA)
  for vader BTL (shared memory) copies when running in user namespaces
  (i.e., containers).  Many thanks to Adrian Reber for raising the
  issue and providing the fix.
- Sending very large MPI messages using the ofi MTL will fail with
  some of the underlying Libfabric transports (e.g., PSM2 with
  messages >=4GB, verbs with messages >=2GB).  Prior version of Open
  MPI failed silently; this version of Open MPI invokes the
  appropriate MPI error handler upon failure.  See
  https://github.com/open-mpi/ompi/issues/7058 for more details.
  Thanks to Emmanuel Thomé for raising the issue.
- Fix case where 0-extent datatypes might be eliminated during
  optimization.  Thanks to Github user @tjahns for raising the issue.
- Ensure that the ``MPIR_Breakpoint`` symbol is not optimized out on
  problematic platforms.
- Fix MPI one-sided 32 bit atomic support.
- Fix OMPIO offset calculations with ``SEEK_END`` and ``SEEK_CUR`` in
  ``MPI_FILE_GET_POSITION``.  Thanks to Wei-keng Liao for raising the
  issue.
- Add "naive" regx component that will never fail, no matter how
  esoteric the hostnames are.
- Fix corner case for datatype extent computations.  Thanks to David
  Dickenson for raising the issue.
- Allow individual jobs to set their map/rank/bind policies when
  running LSF.  Thanks to Nick R. Papior for assistance in solving the
  issue.
- Fix MPI buffered sends with the "cm" PML.
- Properly propagate errors to avoid deadlocks in MPI one-sided operations.
- Update to PMIx v2.2.3.
- Fix data corruption in non-contiguous MPI accumulates over UCX.
- Fix ssh-based tree-based spawning at scale.  Many thanks to Github
  user @zrss for the report and diagnosis.
- Fix the Open MPI RPM spec file to not abort when grep fails.  Thanks
  to Daniel Letai for bringing this to our attention.
- Handle new SLURM CLI options (SLURM 19 deprecated some options that
  Open MPI was using).  Thanks to Jordan Hayes for the report and the
  initial fix.
- OMPI: fix division by zero with an empty file view.
- Also handle ``shmat()``/``shmdt()`` memory patching with OS-bypass networks.
- Add support for unwinding info to all files that are present in the
  stack starting from MPI_Init, which is helpful with parallel
  debuggers.  Thanks to James Clark for the report and initial fix.
- Fixed inadvertant use of bitwise operators in the MPI C++ bindings
  header files.  Thanks to Bert Wesarg for the report and the fix.


Open MPI version 3.1.4
----------------------
:Date: April, 2019

- Fix compile error when configured with ``--enable-mpi-java`` and
  ``--with-devel-headers``.  Thanks to @g-raffy for reporting the issue
  (** also appeared: v3.0.4).
- Only use hugepages with appropriate permissions.  Thanks to Hunter
  Easterday for the fix.
- Fix possible floating point rounding and division issues in OMPIO
  which led to crashes and/or data corruption with very large data.
  Thanks to Axel Huebl and René Widera for identifing the issue,
  supplying and testing the fix (** also appeared: v3.0.4).
- Use ``static_cast<>`` in ``mpi.h`` where appropriate.  Thanks to @shadow-fx
  for identifying the issue (** also appeared: v3.0.4).
- Fix RMA accumulate of non-predefined datatypes with predefined
  operators.  Thanks to Jeff Hammond for raising the issue (** also
  appeared: v3.0.4).
- Fix race condition when closing open file descriptors when launching
  MPI processes.  Thanks to Jason Williams for identifying the issue and
  supplying the fix (** also appeared: v3.0.4).
- Fix support for external PMIx v3.1.x.
- Fix Valgrind warnings for some ``MPI_TYPE_CREATE_*`` functions.  Thanks
  to Risto Toijala for identifying the issue and supplying the fix (**
  also appeared: v3.0.4).
- Fix ``MPI_TYPE_CREATE_F90_{REAL,COMPLEX}`` for r=38 and r=308 (** also
  appeared: v3.0.4).
- Fix assembly issues with old versions of gcc (<6.0.0) that affected
  the stability of shared memory communications (e.g., with the vader
  BTL) (** also appeared: v3.0.4).
- Fix ``MPI_Allreduce`` crashes with some cases in the coll/spacc module.
- Fix the OFI MTL handling of ``MPI_ANY_SOURCE`` (** also appeared:
  v3.0.4).
- Fix noisy errors in the openib BTL with regards to
  ``ibv_exp_query_device()``.  Thanks to Angel Beltre and others who
  reported the issue (** also appeared: v3.0.4).
- Fix zero-size MPI one-sided windows with UCX.


Open MPI version 3.1.3
----------------------
:Date: October, 2018

- Fix race condition in ``MPI_THREAD_MULTIPLE`` support of non-blocking
  send/receive path.
- Fix error handling ``SIGCHLD`` forwarding.
- Add support for ``CHARACTER`` and ``LOGICAL`` Fortran datatypes for ``MPI_SIZEOF``.
- Fix compile error when using OpenJDK 11 to compile the Java bindings.
- Fix crash when using a hostfile with a 'user@host' line.
- Numerous Fortran '08 interface fixes.
- TCP BTL error message fixes.
- OFI MTL now will use any provider other than shm, sockets, tcp, udp, or
  rstream, rather than only supporting gni, psm, and psm2.
- Disable async receive of CUDA buffers by default, fixing a hang
  on large transfers.
- Support the BCM57XXX and BCM58XXX Broadcomm adapters.
- Fix minmax datatype support in ROMIO.
- Bug fixes in vader shared memory transport.
- Support very large buffers with ``MPI_TYPE_VECTOR``.
- Fix hang when launching with mpirun on Cray systems.


Open MPI version 3.1.2
----------------------
:Date: August, 2018

- A subtle race condition bug was discovered in the "vader" BTL
  (shared memory communications) that, in rare instances, can cause
  MPI processes to crash or incorrectly classify (or effectively drop)
  an MPI message sent via shared memory.  If you are using the "ob1"
  PML with "vader" for shared memory communication (note that vader is
  the default for shared memory communication with ob1), you need to
  upgrade to v3.1.2 or later to fix this issue.  You may also upgrade
  to the following versions to fix this issue:

  - Open MPI v2.1.5 (expected end of August, 2018) or later in the
    v2.1.x series
  - Open MPI v3.0.1 (released March, 2018) or later in the v3.0.x
    series

- Assorted Portals 4.0 bug fixes.
- Fix for possible data corruption in MPI_BSEND.
- Move shared memory file for vader btl into /dev/shm on Linux.
- Fix for ``MPI_ISCATTER``/``MPI_ISCATTERV`` Fortran interfaces with ``MPI_IN_PLACE``.
- Upgrade PMIx to v2.1.3.
- Numerous One-sided bug fixes.
- Fix for race condition in uGNI BTL.
- Improve handling of large number of interfaces with TCP BTL.
- Numerous UCX bug fixes.


Open MPI version 3.1.1
----------------------
:Date: June, 2018

- Fix potential hang in UCX PML during ``MPI_Finalize()``
- Update internal PMIx to v2.1.2rc2 to fix forward version compatibility.
- Add new MCA parameter ``osc_sm_backing_store`` to allow users to specify
  where in the filesystem the backing file for the shared memory
  one-sided component should live.  Defaults to ``/dev/shm`` on Linux.
- Fix potential hang on non-x86 platforms when using builds with
  optimization flags turned off.
- Disable osc/pt2pt when using ``MPI_THREAD_MULTIPLE`` due to numerous
  race conditions in the component.
- Fix dummy variable names for the mpi and mpi_f08 Fortran bindings to
  match the MPI standard.  This may break applications which use
  name-based parameters in Fortran which used our internal names
  rather than those documented in the MPI standard.
- Revamp Java detection to properly handle new Java versions which do
  not provide a javah wrapper.
- Fix RMA function signatures for use-mpi-f08 bindings to have the
  asynchonous property on all buffers.
- Improved configure logic for finding the UCX library.


Open MPI version 3.1.0
----------------------
:Date: May, 2018

- Various OpenSHMEM bug fixes.
- Properly handle array_of_commands argument to Fortran version of
  ``MPI_COMM_SPAWN_MULTIPLE``.
- Fix bug with ``MODE_SEQUENTIAL`` and the sharedfp MPI-IO component.
- Use ``javac -h`` instead of ``javah`` when building the Java bindings
  with a recent version of Java.
- Fix mis-handling of jostepid under SLURM that could cause problems
  with PathScale/OmniPath NICs.
- Disable the POWER 7/BE block in configure.  Note that POWER 7/BE is
  still not a supported platform, but it is no longer automatically
  disabled.  See
  https://github.com/open-mpi/ompi/issues/4349#issuecomment-374970982
  for more information.
- The output-filename option for ``mpirun`` is now converted to an
  absolute path before being passed to other nodes.
- Add monitoring component for PML, OSC, and COLL to track data
  movement of MPI applications.  See
  ompi/mca/commmon/monitoring/HowTo_pml_monitoring.tex for more
  information about the monitoring framework.
- Add support for communicator assertions: ``mpi_assert_no_any_tag``,
  ``mpi_assert_no_any_source``, ``mpi_assert_exact_length``, and
  ``mpi_assert_allow_overtaking``.
- Update PMIx to version 2.1.1.
- Update hwloc to 1.11.7.
- Many one-sided behavior fixes.
- Improved performance for Reduce and Allreduce using Rabenseifner's algorithm.
- Revamped mpirun ``--help`` output to make it a bit more manageable.
- Portals4 MTL improvements: Fix race condition in rendezvous protocol and
  retry logic.
- UCX OSC: initial implementation.
- UCX PML improvements: add multi-threading support.
- Yalla PML improvements: Fix error with irregular contiguous datatypes.
- Openib BTL: disable XRC support by default.
- TCP BTL: Add check to detect and ignore connections from processes
  that aren't MPI (such as IDS probes) and verify that source and
  destination are using the same version of Open MPI, fix issue with very
  large message transfer.
- ompi_info parsable output now escapes double quotes in values, and
  also quotes values can contains colons.  Thanks to Lev Givon for the
  suggestion.
- CUDA-aware support can now handle GPUs within a node that do not
  support CUDA IPC.  Earlier versions would get error and abort.
- Add a mca parameter ras_base_launch_orted_on_hn to allow for launching
  MPI processes on the same node where mpirun is executing using a separate
  orte daemon, rather than the mpirun process.   This may be useful to set to
  true when using SLURM, as it improves interoperability with SLURM's signal
  propagation tools.  By default it is set to false, except for Cray XC systems.
- Remove LoadLeveler RAS support.
- Remove IB XRC support from the OpenIB BTL due to lack of support.
- Add functionality for IBM s390 platforms.  Note that regular
  regression testing does not occur on the s390 and it is not
  considered a supported platform.
- Remove support for big endian PowerPC.
- Remove support for XL compilers older than v13.1.
- Remove support for atomic operations using MacOS atomics library.
