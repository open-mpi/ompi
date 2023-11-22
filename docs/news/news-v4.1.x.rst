Open MPI v4.1.x series
======================

This file contains all the NEWS updates for the Open MPI v4.1.x
series, in reverse chronological order.

Open MPI version 4.1.6
----------------------
:Date: September, 2023

- Fix configure issue with XCode 15.
- Update embedded PMIx to 3.2.5.  PMIx 3.2.5 addresses CVE-2023-41915.

  .. note:: Prior versions of Open MPI (and their associated PMIx
            implementations) are **not** impacted by this CVE, because
            Open MPI never uses escalated privileges on behalf of an
            unprivileged user.  We are backporting this change both
            because it is low risk and to avoid alarms from CVE
            scanners.

- Fix issue with buffered sends and MTL-based interfaces (Libfabric,
  PSM, Portals).
- Add missing ``MPI_F_STATUS_SIZE`` to ``mpi.h``.  Thanks to @jprotze for
  reporting the issue.
- Update Fortran ``mpi`` module configure check to be more correct.
  Thanks to Sergey Kosukhin for identifying the issue and supplying
  the fix.
- Update to properly handle PMIx v>=4.2.3.  Thanks to Bruno Chareyre,
  Github user @sukanka, and Christof Koehler for raising the
  compatibility issues and helping test the fixes.
- Fix minor issues and add some minor performance optimizations with
  OFI support.
- Support the ``striping_factor`` and ``striping_unit`` MPI_Info names
  recomended by the MPI standard for parallel IO.
- Fixed some minor issues with UCX support.
- Minor optimization for 0-byte MPI_Alltoallw (i.e., make it a no-op).


Open MPI version 4.1.5
----------------------
:Date: February, 2023

- Fix crash in one-sided applications for certain process layouts.
- Update embedded OpenPMIx to version 3.2.4
- Fix issue building with ``ifort`` on MacOS.
- Backport patches to Libevent for CVE-2016-10195, CVE-2016-10196, and
  CVE-2016-10197.

  .. note:: Open MPI's internal libevent does not use the impacted
            portions of the Libevent code base.

- SHMEM improvements:

  - Fix initializer bugs in SHMEM interface.
  - Fix unsigned type comparisons generating warnings.
  - Fix use after clear issue in ``shmem_ds_reset``.

- UCX improvements

  - Fix memory registration bug that could occur when UCX was built
    but not selected.
  - Reduce overhead of ``add_procs`` with intercommunicators.
  - Enable ``multi_send_nb`` by default.
  - Call ``opal_progress`` while waiting for a UCX fence to complete.

- Fix data corruption bug in osc/rdma component.
- Fix overflow bug in alltoall collective
- Fix crash when displaying topology.
- Add some ``MPI_F_XXX`` constants that were missing from ``mpi.h``.
- coll/ucc bug fixes.


Open MPI version 4.1.4
----------------------
:Date: May, 2022

- Fix possible length integer overflow in numerous non-blocking collective
  operations.
- Fix segmentation fault in UCX if MPI Tool interface is finalized before
  ``MPI_Init`` is called.
- Remove ``/usr/bin/python`` dependency in configure.
- Fix OMPIO issue with long double etypes.
- Update treematch topology component to fix numerous correctness issues.
- Fix memory leak in UCX MCA parameter registration.
- Fix long operation closing file descriptors on non-Linux systems that
  can appear as a hang to users.
- Fix for attribute handling on GCC 11 due to pointer aliasing.
- Fix multithreaded race in UCX PML's datatype handling.
- Fix a correctness issue in CUDA Reduce algorithm.
- Fix compilation issue with CUDA GPUDirect RDMA support.
- Fix to make ``shmem_calloc(..., 0)`` conform to the OpenSHMEM
  specification.
- Add UCC collectives component.
- Fix divide by zero issue in OMPI IO component.
- Fix compile issue with libnl when not in standard search locations.


Open MPI version 4.1.3
----------------------
:Date: March, 2022

- Fixed a seg fault in the ``smcuda`` BTL.  Thanks to Moritz Kreutzer
  and @Stadik for reporting the issue.
- Added support for ``ELEMENTAL`` to the MPI handle comparison
  functions in the ``mpi_f08`` module.  Thanks to Salvatore Filippone
  for raising the issue.
- Minor datatype performance improvements in the CUDA-based code paths.
- Fix ``MPI_ALLTOALLV`` when used with ``MPI_IN_PLACE``.
- Fix ``MPI_BOTTOM`` handling for non-blocking collectives.  Thanks to
  Lisandro Dalcin for reporting the problem.
- Enable OPAL memory hooks by default for UCX.
- Many compiler warnings fixes, particularly for newer versions of
  GCC.
- Fix intercommunicator overflow with large payload collectives.  Also
  fixed ``MPI_REDUCE_SCATTER_BLOCK`` for similar issues with large
  payload collectives.
- Back-port ROMIO 3.3 fix to use stat64() instead of stat() on GPFS.
- Fixed several non-blocking MPI collectives to not round fractions
  based on float precision.
- Fix compile failure for ``--enable-heterogeneous``.  Also updated
  the README to clarify that ``--enable-heterogeneous`` is functional,
  but still not recomended for most environments.
- Minor fixes to OMPIO, including:

  - Fixing the open behavior of shared memory shared file pointers.
    Thanks to Axel Huebl for reporting the issue
  - Fixes to clean up lockfiles when closing files.  Thanks to Eric
    Chamberland for reporting the issue.

- Update LSF configure failure output to be more clear (e.g., on RHEL
  platforms).
- Update ``if_[in|ex]clude`` behavior in ``btl_tcp`` and ``oob_tcp``
  to select *all* interfaces that fall within the specified subnet
  range.


Open MPI version 4.1.2
----------------------
:Date: November, 2021

- ROMIO portability fix for OpenBSD
- Fix handling of ``MPI_IN_PLACE`` with ``MPI_ALLTOALLW`` and improve performance
  of ``MPI_ALLTOALL`` and ``MPI_ALLTOALLV`` for ``MPI_IN_PLACE.``
- Fix one-sided issue with empty groups in Post-Start-Wait-Complete
  synchronization mode.
- Fix Fortran status returns in certain use cases involving
  Generalized Requests
- Romio datatype bug fixes.
- Fix ``oshmem_shmem_finalize()`` when ``main()`` returns non-zero value.
- Fix wrong affinity under LSF with the membind option.
- Fix ``count==0`` cases in ``MPI_REDUCE`` and ``MPI_IREDUCE.``
- Fix ssh launching on Bourne-flavored shells when the user has ``set -u``
  set in their shell startup files.
- Correctly process 0 slots with the ``mpirun --host`` option.
- Ensure to unlink and rebind socket when the Open MPI session
  directory already exists.
- Fix a segv in ``mpirun --disable-dissable-map``.
- Fix a potential hang in the memory hook handling.
- Slight performance improvement in ``MPI_WAITALL`` when running in
  ``MPI_THREAD_MULTIPLE``.
- Fix hcoll datatype mapping and rooted operation behavior.
- Correct some operations modifying ``MPI_Status``.  ``MPI_ERROR`` when it is
  disallowed by the MPI standard.
- UCX updates:

   - Fix datatype reference count issues.
   - Detach dynamic window memory when freeing a window.
   - Fix memory leak in datatype handling.

- Fix various atomic operations issues.
- mpirun: try to set the curses winsize to the pty of the spawned
  task.  Thanks to Stack Overflow user @Seriously for reporting the
  issue.
- PMIx updates:

   - Fix compatibility with external PMIx v4.x installations.
   - Fix handling of PMIx v3.x compiler/linker flags.  Thanks to Erik
     Schnetter for reporting the issue.
   - Skip SLURM-provided PMIx detection when appropriate.  Thanks to
     Alexander Grund for reporting the issue.

- Fix handling by C++ compilers when they #include the STL "<version>"
  header file, which ends up including Open MPI's text VERSION file
  (which is not C code).  Thanks to @srpgilles for reporting the
  issue.
- Fix ``MPI_Op`` support for ``MPI_LONG``.
- Make the MPI C++ bindings library (libmpi_cxx) explicitly depend on
  the OPAL internal library (libopen-pal).  Thanks to Ye Luo for
  reporting the issue.
- Fix configure handling of ``--with-libevent=/usr``.
- Fix memory leak when opening Lustre files.  Thanks to Bert Wesarg
  for submitting the fix.
- Fix ``MPI_SENDRECV_REPLACE`` to correctly process datatype errors.
  Thanks to Lisandro Dalcin for reporting the issue.
- Fix ``MPI_SENDRECV_REPLACE`` to correctly handle large data.  Thanks
  Jakub Benda for reporting this issue and suggesting a fix.
- Add workaround for TCP "dropped connection" errors to drastically
  reduce the possibility of this happening.
- OMPIO updates:

   - Fix handling when AMODE is not set.  Thanks to Rainer Keller for
     reporting the issue and supplying the fix.
   - Fix FBTL "posix" component linking issue.  Thanks for Honggang Li
     for reporting the issue.
   - Fixed segv with ``MPI_FILE_GET_BYTE_OFFSET`` on 0-sized file view.
   - Thanks to GitHub user @shanedsnyder for submitting the issue.

- OFI updates:

   - Multi-plane / Multi-Nic nic selection cleanups
   - Add support for exporting Open MPI memory monitors into
     Libfabric.
   - Ensure that Cisco usNIC devices are never selected by the OFI
     MTL.
   - Fix buffer overflow in OFI networking setup.  Thanks to Alexander
     Grund for reporting the issue and supplying the fix.

- Fix SSEND on tag matching networks.
- Fix error handling in several MPI collectives.
- Fix the ordering of ``MPI_COMM_SPLIT_TYPE``.  Thanks to Wolfgang
  Bangerth for raising the issue.
- No longer install the orted-mpir library (it's an internal / Libtool
  convenience library).  Thanks to Andrew Hesford for the fix.
- PSM2 updates:

   - Allow advanced users to disable PSM2 version checking.
   - Fix to allow non-default installation locations of psm2.h.

Open MPI version 4.1.1
----------------------
:Date: April, 2021

- Fix a number of datatype issues, including an issue with
  improper handling of partial datatypes that could lead to
  an unexpected application failure.
- Change UCX PML to not warn about MPI_Request leaks during
  ``MPI_Finalize()`` by default.  The old behavior can be restored with
  the mca_pml_ucx_request_leak_check MCA parameter.
- Reverted temporary solution that worked around launch issues in
  SLURM v20.11.{0,1,2}. SchedMD encourages users to avoid these
  versions and to upgrade to v20.11.3 or newer.
- Updated PMIx to v3.2.2.
- Fixed configuration issue on Apple Silicon observed with
  Homebrew. Thanks to François-Xavier Coudert for reporting the issue.
- Disabled gcc built-in atomics by default on aarch64 platforms.
- Disabled UCX PML when UCX v1.8.0 is detected. UCX version 1.8.0 has a bug that
  may cause data corruption when its TCP transport is used in conjunction with
  the shared memory transport. UCX versions prior to v1.8.0 are not affected by
  this issue. Thanks to @ksiazekm for reporting the issue.
- Fixed detection of available UCX transports/devices to better inform PML
  prioritization.
- Fixed SLURM support to mark ORTE daemons as non-MPI tasks.
- Improved AVX detection to more accurately detect supported
  platforms.  Also improved the generated AVX code, and switched to
  using word-based MCA params for the op/avx component (vs. numeric
  big flags).
- Improved OFI compatibility support and fixed memory leaks in error
  handling paths.
- Improved HAN collectives with support for Barrier and Scatter. Thanks
  to @EmmanuelBRELLE for these changes and the relevant bug fixes.
- Fixed MPI debugger support (i.e., the ``MPIR_Breakpoint()`` symbol).
  Thanks to @louisespellacy-arm for reporting the issue.
- Fixed ORTE bug that prevented debuggers from reading MPIR_Proctable.
- Removed PML uniformity check from the UCX PML to address performance
  regression.
- Fixed ``MPI_Init_thread(3)`` statement about C++ binding and update
  references about ``MPI_THREAD_MULTIPLE.``  Thanks to Andreas Lösel for
  bringing the outdated docs to our attention.
- Added ``fence_nb`` to Flux PMIx support to address segmentation faults.
- Ensured progress of AIO requests in the POSIX FBTL component to
  prevent exceeding maximum number of pending requests on MacOS.
- Used OPAL's mutli-thread support in the orted to leverage atomic
  operations for object refcounting.
- Fixed segv when launching with static TCP ports.
- Fixed ``--debug-daemons`` mpirun CLI option.
- Fixed bug where mpirun did not honor ``--host`` in a managed job
  allocation.
- Made a managed allocation filter a hostfile/hostlist.
- Fixed bug to marked a generalized request as pending once initiated.
- Fixed external PMIx v4.x check.
- Fixed OSHMEM build with ``--enable-mem-debug``.
- Fixed a performance regression observed with older versions of GCC when
  ``__ATOMIC_SEQ_CST`` is used. Thanks to @BiplabRaut for reporting the issue.
- Fixed buffer allocation bug in the binomial tree scatter algorithm when
  non-contiguous datatypes are used. Thanks to @sadcat11 for reporting the issue.
- Fixed bugs related to the accumulate and atomics functionality in the
  osc/rdma component.
- Fixed race condition in MPI group operations observed with
  ``MPI_THREAD_MULTIPLE`` threading level.
- Fixed a deadlock in the TCP BTL's connection matching logic.
- Fixed pml/ob1 compilation error when CUDA support is enabled.
- Fixed a build issue with Lustre caused by unnecessary header includes.
- Fixed a build issue with IMB LSF workload manager.
- Fixed linker error with UCX SPML.


Open MPI version 4.1.0
----------------------
:Date: December, 2020

- collectives: Add HAN and ADAPT adaptive collectives components.
  Both components are off by default and can be enabled by specifying
  ``mpirun --mca coll_adapt_priority 100 --mca coll_han_priority 100 ...``.
  We intend to enable both by default in Open MPI 5.0.
- OMPIO is now the default for MPI-IO on all filesystems, including
  Lustre (prior to this, ROMIO was the default for Lustre).  Many
  thanks to Mark Dixon for identifying MPI I/O issues and providing
  access to Lustre systems for testing.
- Updates for macOS Big Sur.  Thanks to FX Coudert for reporting this
  issue and pointing to a solution.
- Minor MPI one-sided RDMA performance improvements.
- Fix hcoll ``MPI_SCATTERV`` with ``MPI_IN_PLACE``.
- Add AVX support for MPI collectives.
- Updates to mpirun(1) about "slots" and PE=x values.
- Fix buffer allocation for large environment variables.  Thanks to
  @zrss for reporting the issue.
- Upgrade the embedded OpenPMIx to v3.2.2.
- Take more steps towards creating fully Reproducible builds (see
  https://reproducible-builds.org/).  Thanks Bernhard M. Wiedemann for
  bringing this to our attention.
- Fix issue with extra-long values in MCA files.  Thanks to GitHub
  user @zrss for bringing the issue to our attention.
- UCX: Fix zero-sized datatype transfers.
- Fix ``--cpu-list`` for non-uniform modes.
- Fix issue in PMIx callback caused by missing memory barrier on Arm platforms.
- OFI MTL: Various bug fixes.
- Fixed issue where ``MPI_TYPE_CREATE_RESIZED`` would create a datatype
  with unexpected extent on oddly-aligned datatypes.
- collectives: Adjust default tuning thresholds for many collective
  algorithms
- runtime: fix situation where rank-by argument does not work
- Portals4: Clean up error handling corner cases
- runtime: Remove ``--enable-install-libpmix`` option, which has not
  worked since it was added
- opal: Disable memory patcher component on MacOS
- UCX: Allow UCX 1.8 to be used with the btl uct
- UCX: Replace usage of the deprecated NB API of UCX with NBX
- OMPIO: Add support for the IME file system
- OFI/libfabric: Added support for multiple NICs
- OFI/libfabric: Added support for Scalable Endpoints
- OFI/libfabric: Added btl for one-sided support
- OFI/libfabric: Multiple small bugfixes
- libnbc: Adding numerous performance-improving algorithms
