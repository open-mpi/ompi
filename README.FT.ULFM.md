ULFM Open MPI

This README.md documents the features and options specific to the
**User Level Failure Mitigation (ULFM)** Open MPI implementation.
The upstream (i.e. non-resilient) Open MPI directions also apply to
this release, except when specified here, and can be found in its
README file.

[TOC]

Features
========
This implementation conforms to the User Level Failure Mitigation (ULFM)
MPI Standard draft proposal. The ULFM proposal is developed by the MPI
Forum's Fault Tolerance Working Group to support the continued operation of
MPI programs after crash (node failures) have impacted the execution. The key
principle is that no MPI call (point-to-point, collective, RMA, IO, ...) can
block indefinitely after a failure, but must either succeed or raise an MPI
error.

This implementation produces the three supplementary error codes and five
supplementary interfaces defined in the communicator section of the
[http://fault-tolerance.org/wp-content/uploads/2012/10/20170221-ft.pdf]
(ULFM chapter) standard draft document.

+ `MPIX_ERR_PROC_FAILED` when a process failure prevents the completion of
  an MPI operation.
+ `MPIX_ERR_PROC_FAILED_PENDING` when a potential sender matching a non-blocking
  wildcard source receive has failed.
+ `MPIX_ERR_REVOKED` when one of the ranks in the application has invoked the
  `MPI_Comm_revoke` operation on the communicator.
+ `MPIX_Comm_revoke(MPI_Comm comm)` Interrupts any communication pending on
  the communicator at all ranks.
+ `MPIX_Comm_shrink(MPI_Comm comm, MPI_Comm* newcomm)` creates a new
  communicator where dead processes in comm were removed.
+ `MPIX_Comm_agree(MPI_Comm comm, int *flag)` performs a consensus (i.e. fault
  tolerant allreduce operation) on flag (with the operation bitwise AND).
+ `MPIX_Comm_failure_get_acked(MPI_Comm, MPI_Group*)` obtains the group of
  currently acknowledged failed processes.
+ `MPIX_Comm_failure_ack(MPI_Comm)` acknowledges that the application intends
  to ignore the effect of currently known failures on wildcard receive
  completions and agreement return values.

## Supported Systems
There are several MPI engines available in Open MPI,
notably, PML "ob1", "cm", "ucx", and MTL "ofi", "portals4", "psm2".
At this point, only "ob1" is adapted to support fault tolerance.

"ob1" uses BTL ("Byte Transfer Layer") components for each supported
network. "ob1" supports a variety of networks that can be used in
combination with each other. Collective operations (blocking and
non-blocking) use an optimized implementation on top of "ob1".

- Loopback (send-to-self)
- TCP
- UCT (InfiniBand)
- uGNI (Cray Gemini, Aries)
- Shared Memory (FT supported w/CMA and XPmem; KNEM is untested)
- Tuned and non-blocking collective communications

A full list of supported, untested and disabled components is provided
later in this document.

## More Information
More information (tutorials, examples, build instructions for leading
top500 systems) is also available in the Fault Tolerance Research
Hub website:
  <https://fault-tolerance.org>

## Bibliographic References
If you are looking for, or want to cite a general reference for ULFM,
please use

_Wesley Bland, Aurelien Bouteiller, Thomas Herault, George Bosilca, Jack
J. Dongarra: Post-failure recovery of MPI communication capability: Design
and rationale. IJHPCA 27(3): 244-254 (2013)._

Available from: http://journals.sagepub.com/doi/10.1177/1094342013488238.
___________________________________________________________________________

Building ULFM Open MPI
======================
```bash
./configure --with-ft [...options...]
#    use --with-ft to enable building with ULFM (default),
#        --without-ft to disable it
make [-j N] all install
#    use an integer value of N for parallel builds
```
There are many available configure options (see `./configure --help`
for a full list); a summary of the more commonly used ones is included
in the upstream Open MPI README file. The following paragraph gives a
summary of ULFM Open MPI specific options behavior.

## Configure options
+ `--with-ft=TYPE`
  Specify the type of fault tolerance to enable.  Options: mpi (ULFM MPI
  draft standard).  Fault tolerance build support is **enabled by default**.

+ `--enable-mca-no-build=LIST`
  Comma-separated list of _<type>-<component>_ pairs that will not be built.
  For example, `--enable-mca-no-build=btl-portals,oob-ud` will disable
  building the _portals BTL_ and the _ud OOB_ component. You can use this
  option to disable components that are known to prevent supporting failure
  management __when built-in__. By default, this list is empty, as ulfm has
  a separate mechanism to warn about, or disable loading a component that
  are poorly tested or known to cause breakage when fault-tolerance is
  selected at runtime.

+ `--with-pmi`
  `--with-slurm`
  Force the building of SLURM scheduler support.
  Slurm with fault tolerance is tested. **Do not use `srun`**, otherwise your
  application gets killed by the scheduler upon the first failure. Instead,
  **Use `mpirun` in an `salloc/sbatch`**.

+ `-with-lsf`
  This is untested with fault tolerance.

+ `--with-alps`
  `--with-tm`
  Force the building of PBS/Torque scheduler support.
  PBS is tested with fault tolerance. **Use `mpirun` in a `qsub`
  allocation.**

+ `--disable-oshmem`
  Disable building the OpenSHMEM implementation (by default, it is
  enabled).
  ULFM Fault Tolerance does not apply to OpenSHMEM.
___________________________________________________________________________

## Modified, Untested and Disabled Components
Frameworks and components which are not listed in the following list are
unmodified and support fault tolerance. Listed frameworks may be **modified**
(and work after a failure), **untested** (and work before a failure, but may
malfunction after a failure), or **disabled** (they cause unspecified behavior
all around when FT is enabled).

All runtime disabled components are listed in the `ft-mpi` aggregate MCA param file
`$installdir/share/openmpi/amca-param-sets/ft-mpi`. You can tune the runtime behavior
with ULFM by either setting or unsetting variables in this file (or by overiding
the variable on the command line (e.g., ``--omca btl ofi,self``). Note that if FT is
runtime disabled, these components will load normally (this may change observed
performance when comparing with and without fault tolerance).

- **pml** MPI point-to-point management layer
    - "ob1" modified to **handle errors**
    - "monitoring", "v" unmodified, **untested**
    - "cm", "crcpw", "ucx" **disabled**

- **btl** Point-to-point Byte Transfer Layer
    - "ugni", "uct", "tcp", "sm(+cma,+xpmem)" modified to **handle errors** (removed
      unconditional abort on error, expect performance similar to upstream)
    - "ofi", "portals4", "smcuda", "usnic", "sm(+knem)" unmodified,
      **untested** (may work properly, please report)

- **mtl** Matching transport layer Used for MPI point-to-point messages on
  some types of networks
    - All "mtl" components are **disabled**

- **coll** MPI collective algorithms
    - "base", "basic", "tuned", "nbc" modified to **handle errors**
    - "cuda", "inter", "sync", "sm" unmodified, **untested** (expect correct post-failure behavior)
    - "hcoll", "portals4" unmodified, **disabled** (expect
      unspecified post-failure behavior)

- **osc** MPI one-sided communications
    - Unmodified, **untested** (expect unspecified post-failure behavior)

- **io** MPI I/O and dependent components
    - _fs_ File system functions for MPI I/O
    - _fbtl_ File byte transfer layer: abstraction for individual read/write
      operations for OMPIO
    - _fcoll_ Collective read and write operations for MPI I/O
    - _sharedfp_ Shared file pointer operations for MPI I/O
    - All components in these frameworks are unmodified, **untested**
      (expect clean post-failure abort)

- **vprotocol** Checkpoint/Restart components
    - unmodified **untested**

- **threads** `wait-sync` Multithreaded wait-synchronization object
    - "pthreads" modified to **handle errors** (added a global interrupt to 
       trigger all wait_sync objects)
    - "argotbots", "qthreads" unmodified, **disabled** (expect post-failure
      deadlock)

___________________________________________________________________________

Running ULFM Open MPI
=====================

## Building your application

As ULFM is still an extension to the MPI standard, you will need to
`#include <mpi-ext.h>` in C, or `use mpi_ext` in Fortran to access the
supplementary error codes and functions.

Compile your application as usual, using the provided `mpicc`, `mpif90`, or
`mpicxx` wrappers.

## Running your application

You can launch your application with fault tolerance by simply using the
provided `mpiexec`. Beware that your distribution may already provide a
version of MPI, make sure to set your `PATH` and `LD_LIBRARY_PATH` properly.
Note that fault tolerance is disabled by default in ULFM Open MPI; you can
enable the fault tolerance components by launching your application with
`mpiexec --enable-recovery`.

## Running under a batch scheduler

ULFM can operate under a job/batch scheduler, and is tested routinely with
both ALPS, PBS and Slurm. One difficulty comes from the fact that many job
schedulers will "cleanup" the application as soon as a process fails. In
order to avoid this problem, it is preferred that you use `mpiexec`
within an allocation (e.g. `salloc`, `sbatch`, `qsub`) rather than
a direct launch (e.g. `srun`).

## Run-time tuning knobs

ULFM comes with a variety of knobs for controlling how it runs. The default
parameters are sane and should result in very good performance in most
cases. You can change the default settings with `--omca mpi_ft_foo <value>`
for OMPI options, and with `--prtemca errmgr_detector_bar <value>` for
PRTE options.

### PRTE level options

- `prrte_enable_recovery <true|false> (default: false)` controls automatic
  cleanup of apps with failed processes within mpirun. Enabling this option 
  also enable `mpi_ft_enable`.
- `errmgr_detector_priority <int> (default 1005`) selects the prte-based
  failure detector. Only available when `prte_enable_recovery` is `true`. You
  can set this to `0` when using the (experimental) OMPI detector instead.
- err`mgr_detector_heartbeat_period <float> (default: 5e0)` controls the
  heartbeat period. Recommended value is 1/2 of the timeout.
- `errmgr_detector_heartbeat_timeout <float> (default: 1e1 seconds)` heartbeat
  timeout (i.e. failure detection speed). Recommended value is 2 times
  the heartbeat period. The default setup is tuned for failure-free
  performance at the expense of fault detection reactivity. In environments
  where faults are expected to be common, less conservative values can be
  used (e.g., 100ms); Values lower than the TCP poll rate (typically 10ms)
  can cause false positive.

### OMPI level options

- `mpi_ft_enable <true|false> (default: same as prrte_enable_recovery)`
  permits turning on/off fault tolerance at runtime. When false, failure
  detection is disabled; Interfaces defined by the fault tolerance extensions
  are substituted with dummy non-fault tolerant implementations (e.g.,
  `MPIX_Comm_agree` is implemented with `MPI_Allreduce`); All other controls
  below become irrelevant.
- `mpi_ft_verbose <int> (default: 0)` increases the output of the fault
  tolerance activities. A value of 1 will report detected failures.
- `mpi_ft_detector <true|false> (default: false)`, EXPERIMENTAL, controls
  the activation of the OMPI level failure detector. When this detector
  is turned off, all failure detection is delegated to PRTE (see above).
  The OMPI level fault detector is experimental. There is a tradeoff between
  failure detection accuracy and performance with this detector. Users that
  experience accuracy issues may enable a more precise mode.
  See the tuning knobs below to adjust to taste;
  The OMPI failure detector operates on MPI_COMM_WORLD exclusively.
  Processes connected from MPI_COMM_CONNECT/ACCEPT and MPI_COMM_SPAWN may
  occasionally not be detected when they fail.
- `mpi_ft_detector_thread <true|false> (default: false)` controls the use
  of a thread to emit and receive failure detector's heartbeats. _Setting
  this value to "true" will also set `MPI_THREAD_MULTIPLE` support, which
  has a noticeable effect on latency (typically 1us increase)._ You may
  want to **enable this option if you experience false positive**
  processes incorrectly reported as failed with the OMPI failure detector.
- `mpi_ft_detector_period <float> (default: 3e0 seconds)` heartbeat
  period. Recommended value is 1/3 of the timeout. _Values lower than
  100us may impart a noticeable effect on latency (typically a 3us
  increase)._
- `mpi_ft_detector_timeout <float> (default: 1e1 seconds)` heartbeat
  timeout (i.e. failure detection speed). Recommended value is 3 times
  the heartbeat period.

## Known Limitations in ULFM

- Infiniband support is provided through the OpenIB or UCT BTL, fault
  tolerant operation over the UCX PML is not yet supported.
- TOPO, FILE, RMA are not fault tolerant. They are expected to work properly
  before the occurence of the first failure.

___________________________________________________________________________


Changelog
=========

## ULFM Integrated in Open MPI
ULFM is now integrated in Open MPI. This text will be updated when a new 
Open MPI release is made.

## ULFM Standalone Release 4.0.2u1
This is a stability and upstream parity upgrade. It is based on the most
current Open MPI Release (v4.0.2, October 2019).

- This release is based on Open MPI release v4.0.2 (ompi #cb5f4e737a).
- This release is based on ULFM master (ulfm #0e249ca1).
- New features
    - Support for the UCT BTL enters beta stage.
- Bugfixes
    - High sensitivity to noise in the failure detector.
    - Deadlocks when revoking while BTL progress threads are updating messages.
    - A case where the failure detector would keep observing a dead process forever.
    - Disable the use of external pmix/libevent by default (the internals are modified
      to handle error cases).
    - Clean error paths leaving some rdma registration dangling.
    - Do not remove the orte job/proc session dir prematurely upon error.

## ULFM Standalone Release 4.0.1u1
This is a stability and upstream parity upgrade. It improves stability, 
performance and is based on the most current Open MPI Release (v4.0.1,
May 2019).

- This release is based on Open MPI release v4.0.1 (ompi #b780667).
- This release is based on ULFM master (ulfm #cf8dc43f).
- New features
    - Addition of the `MPI_Comm_is_revoked` function
    - Renamed `ftbasic` collective component to `ftagree`
    - Restored the `pcollreq` extension
- Bugfixes
    - Failures of node-local siblings were not always detected
    - Failure propagation and detection was slowed down by trying to 
      notify known dead processes
    - There were deadlocks in multithreaded programs
    - There were issues with PMPI when compiling Fortran Interfaces
    - There were deadlocks on OS-X

## ULFM Standalone Release 2.1
This release is a bugfix and upstream parity upgrade. It improves stability,
performance and is based on the most current Open MPI master (November 2018).

- ULFM is now based upon Open MPI master branch (#37954b5f).
- ULFM tuning MCA parameters are exposed by `ompi_info`.
- Fortran 90 bindings have been updated
- Bugfixes:
    - Correct the behavior of process placement during an MPI_COMM_SPAWN when 
      some slots were occcupied by failed processes.
    - MPI_COMM_SPAWN accepts process placement directives in the Info object.
    - Fixed deadlocks in some NBC collective operations.
    - Crashes and deadlocks in MPI_FINALIZE have been resolved.
    - Any-source requests that returned with an error status of 
      MPIX_PROC_FAILED_PENDING can now correctly complete during 
      later MPI_WAIT/TEST.

## ULFM Standalone Release 2.0
Focus has been toward integration with current Open MPI master (November 2017),
performance, and stability.

- ULFM is now based upon Open MPI master branch (#689f1be9). It will be
  regularly updated until it will eventually be merged.
- Fault Tolerance is enabled by default and is controlled with MCA variables.
- Added support for multithreaded modes (MPI_THREAD_MULTIPLE, etc.)
- Added support for non-blocking collective operations (NBC).
- Added support for CMA shared memory transport (Vader).
- Added support for advanced failure detection at the MPI level.
  Implements the algorithm described in "Failure detection and
  propagation in HPC systems." <https://doi.org/10.1109/SC.2016.26>.
- Removed the need for special handling of CID allocation.
- Non-usable components are automatically removed from the build during configure
- RMA, FILES, and TOPO components are enabled by default, and usage in a fault
  tolerant execution warns that they may cause undefined behavior after a failure.
- Bugfixes:
    - Code cleanup and performance cleanup in non-FT builds; --without-ft at
      configure time gives an almost stock Open MPI.
    - Code cleanup and performance cleanup in FT builds with FT runtime disabled;
      --mca ft_enable_mpi false thoroughly disables FT runtime activities.
    - Some error cases would return ERR_PENDING instead of ERR_PROC_FAILED in
      collective operations.
    - Some test could set ERR_PENDING or ERR_PROC_FAILED instead of
      ERR_PROC_FAILED_PENDING for ANY_SOURCE receptions.
___________________________________________________________________________

## ULFM Standalone Release 1.1
Focus has been toward improving stability, feature coverage for intercomms,
and following the updated specification for MPI_ERR_PROC_FAILED_PENDING.

- Forked from Open MPI 1.5.5 devel branch
- Addition of the MPI_ERR_PROC_FAILED_PENDING error code, as per newer specification
  revision. Properly returned from point-to-point, non-blocking ANY_SOURCE operations.
- Alias MPI_ERR_PROC_FAILED, MPI_ERR_PROC_FAILED_PENDING and MPI_ERR_REVOKED to the
  corresponding standard blessed -extension- names MPIX_ERR_xxx.
- Support for Intercommunicators:
    - Support for the blocking version of the agreement, MPI_COMM_AGREE on Intercommunicators.
    - MPI_COMM_REVOKE tested on intercommunicators.
- Disabled completely (.ompi_ignore) many untested components.
- Changed the default ORTE failure notification propagation aggregation delay from 1s to 25ms.
- Added an OMPI internal failure propagator; failure propagation between SM domains is now
  immediate.
- Bugfixes:
    - SendRecv would not always report MPI_ERR_PROC_FAILED correctly.
    - SendRecv could incorrectly update the status with errors pertaining to the Send portion
      of the Sendrecv.
    - Revoked send operations are now always completed or remote cancelled and may not
      deadlock anymore.
    - Cancelled send operations to a dead peer will not trigger an assert when the BTL reports
      that same failure.
    - Repeat calls to operations returning MPI_ERR_PROC_FAILED will eventually return
      MPI_ERR_REVOKED when another process revokes the communicator.
___________________________________________________________________________

## ULFM Standalone Release 1.0
Focus has been toward improving performance, both before and after the occurence of failures.
The list of new features includes:

- Support for the non-blocking version of the agreement, MPI_COMM_IAGREE.
- Compliance with the latest ULFM specification draft. In particular, the
  MPI_COMM_(I)AGREE semantic has changed.
- New algorithm to perform agreements, with a truly logarithmic complexity in number of
  ranks, which translates into huge performance boosts in MPI_COMM_(I)AGREE and
  MPI_COMM_SHRINK.
- New algorithm to perform communicator revocation. MPI_COMM_REVOKE performs a reliable
  broadcast with a fixed maximum output degree, which scales logarithmically with the
  number of ranks.
- Improved support for our traditional network layer:
    - TCP: fully tested
    - SM: fully tested (with the exception of XPMEM, which remains unsupported)
- Added support for High Performance networks
    - Open IB: reasonably tested
    - uGNI: reasonably tested
- The tuned collective module is now enabled by default (reasonably tested), expect a
  huge performance boost compared to the former basic default setting
    - Back-ported PBS/ALPS fixes from Open MPI
    - Back-ported OpenIB bug/performance fixes from Open MPI
    - Improve Context ID allocation algorithm to reduce overheads of Shrink
    - Miscellaneous bug fixes
___________________________________________________________________________

## Binary Compatibility
ULFM Open MPI is binary compatible with any version of Open MPI compatible
with the underlying Open MPI master branch or release (see
the binary compatibility and version number section in the upstream Open MPI
README). That is, applications compiled with a compatible Open MPI can run
with the ULFM Open MPI `mpirun` and MPI libraries. Conversely, _as long as
the application does not employ one of the MPIX functions_, which are
exclusively defined in ULFM Open MPI, an application compiled with
ULFM Open MPI can be launched with a compatible Open MPI `mpirun` and run
with the non-fault tolerant MPI library.
___________________________________________________________________________

Contacting the Authors
======================
Found a bug?  Got a question?  Want to make a suggestion?  Want to
contribute to ULFM Open MPI?  Working on a cool use-case?
Please let us know!

The best way to report bugs, send comments, or ask questions is to
sign up on the user's mailing list:
  <ulfm+subscribe@googlegroups.com>

Because of spam, only subscribers are allowed to post to these lists
(ensure that you subscribe with and post from exactly the same e-mail
address -- joe@example.com is considered different than
joe@mycomputer.example.com!).  Visit these pages to subscribe to the
lists:
  <https://groups.google.com/forum/#!forum/ulfm>

When submitting questions and problems, be sure to include as much
extra information as possible.  This web page details all the
information that we request in order to provide assistance:
  <http://www.open-mpi.org/community/help/>

Thanks for your time.
___________________________________________________________________________

Copyright
=========

```
Copyright (c) 2012-2020 The University of Tennessee and The University
                        of Tennessee Research Foundation.  All rights
                        reserved.

$COPYRIGHT$

Additional copyrights may follow

$HEADER$
```
