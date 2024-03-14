Open MPI v4.0.x series
======================

This file contains all the NEWS updates for the Open MPI v4.0.x
series, in reverse chronological order.

Open MPI version 4.0.6
----------------------
:Date: March, 2021

- Update embedded PMIx to 3.2.2.  This update addresses several
  ``MPI_COMM_SPAWN`` problems.
- Fix a problem when using Flux PMI and UCX.  Thanks to Sami Ilvonen
  for reporting and supplying a fix.
- Fix a problem with MPIR breakpoint being compiled out using PGI
  compilers.  Thanks to @louisespellacy-arm for reporting.
- Fix some ROMIO issues when using Lustre.  Thanks to Mark Dixon for
  reporting.
- Fix a problem using an external PMIx 4 to build Open MPI 4.0.x.
- Fix a compile problem when using the enable-timing configure option
  and UCX.  Thanks to Jan Bierbaum for reporting.
- Fix a symbol name collision when using the Cray compiler to build
  Open SHMEM.  Thanks to Pak Lui for reporting and fixing.
- Correct an issue encountered when building Open MPI under OSX Big Sur.
  Thanks to FX Coudert for reporting.
- Various fixes to the OFI MTL.
- Fix an issue with allocation of sufficient memory for parsing long
  environment variable values.  Thanks to @zrss for reporting.
- Improve reproducibility of builds to assist Open MPI packages.
  Thanks to Bernhard Wiedmann for bringing this to our attention.


Open MPI version 4.0.5
----------------------
:Date: August, 2020

- Fix a problem with MPI RMA compare and swap operations.  Thanks
  to Wojciech Chlapek for reporting.
- Disable binding of MPI processes to system resources by Open MPI
  if an application is launched using SLURM's srun command.
- Disable building of the Fortran mpi_f08 module when configuring
  Open MPI with default 8 byte Fortran integer size.  Thanks to
  @ahcien for reporting.
- Fix a problem with mpirun when the ``--map-by`` option is used.
  Thanks to Wenbin Lyu for reporting.
- Fix some issues with MPI one-sided operations uncovered using Global
  Arrays regression test-suite.  Thanks to @bjpalmer for reporting.
- Fix a problem with make check when using the PGI compiler.  Thanks to
  Carl Ponder for reporting.
- Fix a problem with ``MPI_FILE_READ_AT_ALL`` that could lead to application
  hangs under certain circumstances.  Thanks to Scot Breitenfeld for
  reporting.
- Fix a problem building C++ applications with newer versions of GCC.
  Thanks to Constantine Khrulev for reporting.


Open MPI version 4.0.4
----------------------
:Date: June, 2020

- Fix a memory patcher issue intercepting shmat and shmdt.  This was
  observed on RHEL 8.x ppc64le (see README for more info).
- Fix an illegal access issue caught using gcc's address sanitizer.
  Thanks to  Georg Geiser for reporting.
- Add checks to avoid conflicts with a libevent library shipped with LSF.
- Switch to linking against libevent_core rather than libevent, if present.
- Add improved support for UCX 1.9 and later.
- Fix an ABI compatibility issue with the Fortran 2008 bindings.
  Thanks to Alastair McKinstry for reporting.
- Fix an issue with rpath of ``/usr/lib64`` when building OMPI on
  systems with Lustre.  Thanks to David Shrader for reporting.
- Fix a memory leak occurring with certain MPI RMA operations.
- Fix an issue with ORTE's mapping of MPI processes to resources.
  Thanks to Alex Margolin for reporting and providing a fix.
- Correct a problem with incorrect error codes being returned
  by OMPI MPI_T functions.
- Fix an issue with debugger tools not being able to attach
  to mpirun more than once.  Thanks to Gregory Lee for reporting.
- Fix an issue with the Fortran compiler wrappers when using
  NAG compilers.  Thanks to Peter Brady for reporting.
- Fix an issue with the ORTE ssh based process launcher at scale.
  Thanks to Benjamín Hernández for reporting.
- Address an issue when using shared MPI I/O operations.  OMPIO will
  now successfully return from the file open statement but will
  raise an error if the file system does not supported shared I/O
  operations.  Thanks to Romain Hild for reporting.
- Fix an issue with ``MPI_WIN_DETACH.``  Thanks to Thomas Naughton for reporting.


Open MPI version 4.0.3
----------------------
:Date: March, 2020

- Update embedded PMIx to 3.1.5
- Add support for Mellanox ConnectX-6.
- Fix an issue in OpenMPI IO when using shared file pointers.
  Thanks to Romain Hild for reporting.
- Fix a problem with Open MPI using a previously installed
  Fortran mpi module during compilation.  Thanks to Marcin
  Mielniczuk for reporting
- Fix a problem with Fortran compiler wrappers ignoring use of
  disable-wrapper-runpath configure option.  Thanks to David
  Shrader for reporting.
- Fixed an issue with trying to use mpirun on systems where neither
  ssh nor rsh is installed.
- Address some problems found when using XPMEM for intra-node message
  transport.
- Improve dimensions returned by MPI_Dims_create for certain
  cases.  Thanks to @aw32 for reporting.
- Fix an issue when sending messages larger than 4GB. Thanks to
  Philip Salzmann for reporting this issue.
- Add ability to specify alternative module file path using
  Open MPI's RPM spec file.  Thanks to @jschwartz-cray for reporting.
- Clarify use of ``--with-hwloc`` configuration option in the README.
  Thanks to Marcin Mielniczuk for raising this documentation issue.
- Fix an issue with shmem_atomic_set.  Thanks to Sameh Sharkawi for reporting.
- Fix a problem with ``MPI_Neighbor_alltoall(v,w)`` for cartesian communicators
  with cyclic boundary conditions.  Thanks to Ralph Rabenseifner and
  Tony Skjellum for reporting.
- Fix an issue using Open MPIO on 32 bit systems.  Thanks to
  Orion Poplawski for reporting.
- Fix an issue with NetCDF test deadlocking when using the vulcan
  Open MPIO component.  Thanks to Orion Poplawski for reporting.
- Fix an issue with the ``mpi_yield_when_idle`` parameter being ignored
  when set in the Open MPI MCA parameter configuration file.
  Thanks to @iassiour for reporting.
- Address an issue with Open MPIO when writing/reading more than 2GB
  in an operation.  Thanks to Richard Warren for reporting.


Open MPI version 4.0.2
----------------------
:Date: September, 2019

- Update embedded PMIx to 3.1.4
- Enhance Open MPI to detect when processes are running in
  different name spaces on the same node, in which case the
  vader CMA single copy mechanism is disabled.  Thanks
  to Adrian Reber for reporting and providing a fix.
- Fix an issue with ORTE job tree launch mechanism.  Thanks
  to @lanyangyang for reporting.
- Fix an issue with env processing when running as root.
  Thanks to Simon Byrne for reporting and providing a fix.
- Fix Fortran ``MPI_FILE_GET_POSITION`` return code bug.
  Thanks to Wei-Keng Liao for reporting.
- Fix user defined datatypes/ops leak in nonblocking base collective
  component.  Thanks to Andrey Maslennikov for verifying fix.
- Fixed shared memory not working with spawned processes.
  Thanks to @rodarima for reporting.
- Fix data corruption of overlapping datatypes on sends.
  Thanks to DKRZ for reporting.
- Fix segfault in oob_tcp component on close with active listeners.
  Thanks to Orivej Desh for reporting and providing a fix.
- Fix divide by zero segfault in ompio.
  Thanks to @haraldkl for reporting and providing a fix.
- Fix finalize of flux compnents.
  Thanks to Stephen Herbein and Jim Garlick for providing a fix.
- Fix osc_rdma_acc_single_intrinsic regression.
  Thanks to Joseph Schuchart for reporting and providing a fix.
- Fix hostnames with large integers.
  Thanks to @perrynzhou for reporting and providing a fix.
- Fix Deadlock in ``MPI_Fetch_and_op`` when using UCX
  Thanks to Joseph Schuchart for reporting.
- Fix the SLURM plm for mpirun-based launching.
  Thanks to Jordon Hayes for reporting and providing a fix.
- Prevent grep failure in rpmbuild from aborting.
  Thanks to Daniel Letai for reporting.
- Fix btl/vader finalize sequence.
  Thanks to Daniel Vollmer for reporting.
- Fix pml/ob1 local handle sent during PUT control message.
  Thanks to @EmmanuelBRELLE for reporting and providing a fix.
- Fix Memory leak with persistent MPI sends and the ob1 "get" protocol.
  Thanks to @s-kuberski for reporting.
- v4.0.x: mpi: mark ``MPI_COMBINER_{HVECTOR,HINDEXED,STRUCT}_INTEGER``
  removed unless configured with ``--enable-mpi1-compatibility``
- Fix make-authors.pl when run in a git submodule.
  Thanks to Michael Heinz for reporting and providing a fix.
- Fix deadlock with ``mpi_assert_allow_overtaking`` in MPI_Issend.
  Thanks to Joseph Schuchart and George Bosilca for reporting.
- Add compilation flag to allow unwinding through files that are
  present in the stack when attaching with MPIR.
  Thanks to James A Clark for reporting and providing a fix.

.. admonition:: Known Issue:
   :class: knownissue

   There is a known issue with the OFI libfabric and PSM2 MTLs when trying to send
   very long (> 4 GBytes) messages.  In this release, these MTLs will catch
   this case and abort the transfer.  A future release will provide a
   better solution to this issue.


Open MPI version 4.0.1
----------------------
:Date: March, 2019

- Update embedded PMIx to 3.1.2.
- Fix an issue with Vader (shared-memory) transport on OS-X. Thanks
  to Daniel Vollmer for reporting.
- Fix a problem with the usNIC BTL Makefile.  Thanks to George Marselis
  for reporting.
- Fix an issue when using ``--enable-visibility`` configure option
  and older versions of hwloc.  Thanks to Ben Menadue for reporting
  and providing a fix.
- Fix an issue with ``MPI_WIN_CREATE_DYNAMIC`` and ``MPI_GET`` from self.
  Thanks to Bart Janssens for reporting.
- Fix an issue of excessive compiler warning messages from mpi.h
  when using newer C++ compilers.  Thanks to @Shadow-fax for
  reporting.
- Fix a problem when building Open MPI using clang 5.0.
- Fix a problem with ``MPI_WIN_CREATE`` when using UCX.  Thanks
  to Adam Simpson for reporting.
- Fix a memory leak encountered for certain MPI datatype
  destructor operations.  Thanks to Axel Huebl for reporting.
- Fix several problems with MPI RMA accumulate operations.
  Thanks to Jeff Hammond for reporting.
- Fix possible race condition in closing some file descriptors
  during job launch using mpirun.  Thanks to Jason Williams
  for reporting and providing a fix.
- Fix a problem in OMPIO for large individual write operations.
  Thanks to Axel Huebl for reporting.
- Fix a problem with parsing of map-by ppr options to mpirun.
  Thanks to David Rich for reporting.
- Fix a problem observed when using the mpool hugepage component.  Thanks
  to Hunter Easterday for reporting and fixing.
- Fix valgrind warning generated when invoking certain MPI Fortran
  data type creation functions.  Thanks to @rtoijala for reporting.
- Fix a problem when trying to build with a PMIX 3.1 or newer
  release.  Thanks to Alastair McKinstry for reporting.
- Fix a problem encountered with building MPI F08 module files.
  Thanks to Igor Andriyash and Axel Huebl for reporting.
- Fix two memory leaks encountered for certain MPI-RMA usage patterns.
  Thanks to Joseph Schuchart for reporting and fixing.
- Fix a problem with the ORTE ``rmaps_base_oversubscribe`` MCA paramater.
  Thanks to @iassiour for reporting.
- Fix a problem with UCX PML default error handler for MPI communicators.
  Thanks to Marcin Krotkiewski for reporting.
- Fix various issues with OMPIO uncovered by the testmpio test suite.


Open MPI version 4.0.0
----------------------
:Date: September, 2018

- OSHMEM updated to the OpenSHMEM 1.4 API.
- Do not build OpenSHMEM layer when there are no SPMLs available.
  Currently, this means the OpenSHMEM layer will only build if
  a MXM or UCX library is found.
- A UCX BTL was added for enhanced MPI RMA support using UCX
- With this release,  OpenIB BTL now only supports iWarp and RoCE by default.
- Updated internal HWLOC to 2.0.2
- Updated internal PMIx to 3.0.2
- Change the priority for selecting external verses internal HWLOC
  and PMIx packages to build.  Starting with this release, configure
  by default selects available external HWLOC and PMIx packages over
  the internal ones.
- Updated internal ROMIO to 3.2.1.
- Removed support for the MXM MTL.
- Removed support for SCIF.
- Improved CUDA support when using UCX.
- Enable use of CUDA allocated buffers for OMPIO.
- Improved support for two phase MPI I/O operations when using OMPIO.
- Added support for Software-based Performance Counters, see
  https://github.com/davideberius/ompi/wiki/How-to-Use-Software-Based-Performance-Counters-(SPCs)-in-Open-MPI
- Change MTL OFI from opting-IN on "psm,psm2,gni" to opting-OUT on
  "shm,sockets,tcp,udp,rstream"
- Various improvements to MPI RMA performance when using RDMA
  capable interconnects.
- Update memkind component to use the memkind 1.6 public API.
- Fix a problem with javadoc builds using OpenJDK 11.  Thanks to
  Siegmar Gross for reporting.
- Fix a memory leak using UCX.  Thanks to Charles Taylor for reporting.
- Fix hangs in ``MPI_Finalize`` when using UCX.
- Fix a problem with building Open MPI using an external PMIx 2.1.2
  library.  Thanks to Marcin Krotkiewski for reporting.
- Fix race conditions in Vader (shared memory) transport.
- Fix problems with use of newer map-by mpirun options.  Thanks to
  Tony Reina for reporting.
- Fix rank-by algorithms to properly rank by object and span
- Allow for running as root of two environment variables are set.
  Requested by Axel Huebl.
- Fix a problem with building the Java bindings when using Java 10.
  Thanks to Bryce Glover for reporting.
- Fix a problem with ORTE not reporting error messages if an application
  terminated normally but exited with non-zero error code.  Thanks to
  Emre Brookes for reporting.
