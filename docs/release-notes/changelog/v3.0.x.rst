Open MPI v3.0.x series
======================

This file contains all the NEWS updates for the Open MPI v3.0.x
series, in reverse chronological order.

Open MPI version 3.0.6
----------------------
:Date: March, 2020

- Fix one-sided shared memory window configuration bug.
- Fix support for PGI'18 compiler.
- Fix run-time linker issues with OMPIO on newer Linux distros.
- Allow the user to override modulefile_path in the Open MPI SRPM,
  even if ``install_in_opt`` is set to 1.
- Properly detect ConnectX-6 HCAs in the openib BTL.
- Fix segfault in the MTL/OFI initialization for large jobs.
- Fix various portals4 control flow bugs.
- Fix communications ordering for alltoall and Cartesian neighborhood
  collectives.
- Fix an infinite recursion crash in the memory patcher on systems
  with glibc v2.26 or later (e.g., Ubuntu 18.04) when using certain
  OS-bypass interconnects.


Open MPI version 3.0.5
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
- Ensure that the MPIR_Breakpoint symbol is not optimized out on
  problematic platforms.
- Fix OMPIO offset calculations with ``SEEK_END`` and ``SEEK_CUR`` in
  ``MPI_FILE_GET_POSITION``.  Thanks to Wei-keng Liao for raising the
  issue.
- Fix corner case for datatype extent computations.  Thanks to David
  Dickenson for raising the issue.
- Fix MPI buffered sends with the "cm" PML.
- Update to PMIx v2.2.3.
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
  stack starting from ``MPI_Init``, which is helpful with parallel
  debuggers.  Thanks to James Clark for the report and initial fix.
- Fixed inadvertant use of bitwise operators in the MPI C++ bindings
  header files.  Thanks to Bert Wesarg for the report and the fix.
- Added configure option ``--disable-wrappers-runpath`` (alongside the
  already-existing ``--disable-wrappers-rpath`` option) to prevent Open
  MPI's configure script from automatically adding runpath CLI options
  to the wrapper compilers.


Open MPI version 3.0.4
----------------------
:Date: April, 2019

- Fix compile error when configured with ``--enable-mpi-java`` and
  ``--with-devel-headers``.  Thanks to @g-raffy for reporting the issue.
- Fix possible floating point rounding and division issues in OMPIO
  which led to crashes and/or data corruption with very large data.
  Thanks to Axel Huebl and René Widera for identifing the issue,
  supplying and testing the fix (** also appeared: v3.0.4).
- Use ``static_cast<>`` in ``mpi.h`` where appropriate.  Thanks to @shadow-fx
  for identifying the issue.
- Fix datatype issue with RMA accumulate.  Thanks to Jeff Hammond for
  raising the issue.
- Fix RMA accumulate of non-predefined datatypes with predefined
  operators.  Thanks to Jeff Hammond for raising the issue.
- Fix race condition when closing open file descriptors when launching
  MPI processes.  Thanks to Jason Williams for identifying the issue and
  supplying the fix.
- Fix Valgrind warnings for some ``MPI_TYPE_CREATE_*`` functions.  Thanks
  to Risto Toijala for identifying the issue and supplying the fix.
- Fix ``MPI_TYPE_CREATE_F90_{REAL,COMPLEX}`` for r=38 and r=308.
- Fix assembly issues with old versions of gcc (<6.0.0) that affected
  the stability of shared memory communications (e.g., with the vader
  BTL).
- Fix the OFI MTL handling of ``MPI_ANY_SOURCE``.
- Fix noisy errors in the openib BTL with regards to
  ``ibv_exp_query_device()``.  Thanks to Angel Beltre and others who
  reported the issue.


Open MPI version 3.0.3
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
- Bug fixes in OFI MTL.
- Assorted Portals 4.0 bug fixes.
- Fix for possible data corruption in ``MPI_BSEND``.
- Move shared memory file for vader btl into ``/dev/shm`` on Linux.
- Fix for ``MPI_ISCATTER``/``MPI_ISCATTERV`` Fortran interfaces with ``MPI_IN_PLACE``.
- Upgrade PMIx to v2.1.4.
- Fix for Power9 built-in atomics.
- Numerous One-sided bug fixes.
- Fix for race condition in uGNI BTL.
- Improve handling of large number of interfaces with TCP BTL.
- Numerous UCX bug fixes.
- Add support for QLogic and Broadcom Cumulus RoCE HCAs to Open IB BTL.
- Add patcher support for aarch64.
- Fix hang on Power and ARM when Open MPI was built with low compiler
  optimization settings.


Open MPI version 3.0.2
----------------------
:Date: June, 2018

- Disable osc/pt2pt when using ``MPI_THREAD_MULTIPLE`` due to numerous
  race conditions in the component.
- Fix dummy variable names for the mpi and mpi_f08 Fortran bindings to
  match the MPI standard.  This may break applications which use
  name-based parameters in Fortran which used our internal names
  rather than those documented in the MPI standard.
- Fixed ``MPI_SIZEOF`` in the "mpi" Fortran module for the NAG compiler.
- Fix RMA function signatures for ``use-mpi-f08`` bindings to have the
  asynchonous property on all buffers.
- Fix Fortran ``MPI_COMM_SPAWN_MULTIPLE`` to properly follow the count
  length argument when parsing the array_of_commands variable.
- Revamp Java detection to properly handle new Java versions which do
  not provide a javah wrapper.
- Improved configure logic for finding the UCX library.
- Add support for HDR InfiniBand link speeds.
- Disable the POWER 7/BE block in configure.  Note that POWER 7/BE is
  still not a supported platform, but it is no longer automatically
  disabled.  See
  https://github.com/open-mpi/ompi/issues/4349#issuecomment-374970982
  for more information.


Open MPI version 3.0.1
----------------------
:Date: March, 2018

- Fix ability to attach parallel debuggers to MPI processes.
- Fix a number of issues in MPI I/O found by the HDF5 test suite.
- Fix (extremely) large message transfers with shared memory.
- Fix out of sequence bug in multi-NIC configurations.
- Fix stdin redirection bug that could result in lost input.
- Disable the LSF launcher if CSM is detected.
- Plug a memory leak in ``MPI_Mem_free()``.  Thanks to Philip Blakely for reporting.
- Fix the tree spawn operation when the number of nodes is larger than the radix.
  Thanks to Carlos Eduardo de Andrade for reporting.
- Fix Fortran 2008 macro in MPI extensions.  Thanks to Nathan T. Weeks for
  reporting.
- Add UCX to list of interfaces that OpenSHMEM will use by default.
- Add ``--{enable|disable}-show-load-errors-by-default`` to control
  default behavior of the load errors option.
- OFI MTL improvements: handle empty completion queues properly, fix
  incorrect error message around ``fi_getinfo()``, use default progress
  option for provider by default, Add support for reading multiple
  CQ events in ofi_progress.
- PSM2 MTL improvements: Allow use of GPU buffers, thread fixes.
- Numerous corrections to memchecker behavior.
- Add a mca parameter ``ras_base_launch_orted_on_hn`` to allow for launching
  MPI processes on the same node where mpirun is executing using a separate
  orte daemon, rather than the mpirun process.   This may be useful to set to
  true when using SLURM, as it improves interoperability with SLURM's signal
  propagation tools.  By default it is set to false, except for Cray XC systems.
- Fix a problem reported on the mailing separately by Kevin McGrattan and Stephen
  Guzik about consistency issues on NFS file systems when using OMPIO. This fix
  also introduces a new mca parameter ``fs_ufs_lock_algorithm`` which allows to
  control the locking algorithm used by ompio for read/write operations. By
  default, ompio does not perfom locking on local UNIX file systems, locks the
  entire file per operation on NFS file systems, and selective byte-range
  locking on other distributed file systems.
- Add an mca parameter ``pmix_server_usock_connections`` to allow mpirun to
  support applications statically built against the Open MPI v2.x release,
  or installed in a container along with the Open MPI v2.x libraries. It is
  set to false by default.


Open MPI version 3.0.0
----------------------
:Date: September, 2017

.. important:: Major new features:

   - Use UCX allocator for OSHMEM symmetric heap allocations to optimize intra-node
     data transfers.  UCX SPML only.
   - Use UCX multi-threaded API in the UCX PML.  Requires UCX 1.0 or later.
   - Added support for Flux PMI
   - Update embedded PMIx to version 2.1.0
   - Update embedded hwloc to version 1.11.7

- Per Open MPI's versioning scheme (see the README), increasing the
  major version number to 3 indicates that this version is not
  ABI-compatible with prior versions of Open MPI. In addition, there may
  be differences in MCA parameter names and defaults from previous releases.
  Command line options for mpirun and other commands may also differ from
  previous versions. You will need to recompile MPI and OpenSHMEM applications
  to work with this version of Open MPI.
- With this release, Open MPI supports ``MPI_THREAD_MULTIPLE`` by default.
- New configure options have been added to specify the locations of libnl
  and zlib.
- A new configure option has been added to request Flux PMI support.
- The help menu for mpirun and related commands is now context based.
  ``mpirun --help compatibility`` generates the help menu in the same format
  as previous releases.

.. attention:: Removed legacy support:

   - AIX is no longer supported.
   - Loadlever is no longer supported.
   - OpenSHMEM currently supports the UCX and MXM transports via the ucx and ikrit
     SPMLs respectively.
   - Remove IB XRC support from the OpenIB BTL due to lack of support.
   - Remove support for big endian PowerPC.
   - Remove support for XL compilers older than v13.1

.. note:: Known issues:

   - MPI_Connect/accept between applications started by different mpirun
     commands will fail, even if ompi-server is running.
