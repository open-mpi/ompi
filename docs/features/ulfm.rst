.. _ulfm-label:

User-Level Fault Mitigation (ULFM)
==================================

This chapter documents the features and options specific to the **User
Level Failure Mitigation (ULFM)** Open MPI implementation.

Quick Start
-----------

This is an extremely terse summary of how to use ULFM:

.. code-block::

  shell$ ./configure --with-ft=ulfm [...options...]
  shell$ make [-j N] all install
  shell$ mpicc my-ft-program.c -o my-ft-program
  shell$ mpirun -n 4 --with-ft ulfm my-ft-program

Features
--------

This implementation conforms to the User Level Failure Mitigation
(ULFM) MPI Standard draft proposal. The ULFM proposal is developed by
the MPI Forum's Fault Tolerance Working Group to support the continued
operation of MPI programs after any type of failures, hard or soft,
have impacted the execution. The key principle is that no MPI call
(point-to-point, collective, RMA, IO, ...) can block indefinitely
after a failure, but must either succeed or raise an MPI
error. Accordingly, the errors are not all fatal, the MPI
implementations will do a best-effort approach to maintain the
execution environment up and running.

This implementation produces the three supplementary error codes and
five supplementary interfaces defined in the communicator section of
the `ULFM chapter
<https://fault-tolerance.org/wp-content/uploads/2012/10/20170221-ft.pdf>`_
standard draft document.

* ``MPIX_ERR_PROC_FAILED`` when a process failure prevents the
  completion of an MPI operation (error code).
* ``MPIX_ERR_PROC_FAILED_PENDING`` when a potential sender matching a
  non-blocking wildcard source receive has failed (error code).
* ``MPIX_ERR_REVOKED`` when the communicator is revoked (error
  code).
* ``MPIX_Comm_revoke(MPI_Comm comm)`` Interrupts any communication
  pending on the communicator at all ranks (API). See :ref:`MPIX_Comm_revoke`.
* ``MPIX_Comm_is_revoked(MPI_Comm comm, int *flag)`` Test if a Communicator
  is currently revoked (API). See :ref:`MPIX_Comm_is_revoked`.
* ``MPIX_Comm_shrink(MPI_Comm comm, MPI_Comm* newcomm)`` creates a new
  communicator where dead processes in comm were removed, and the
  remaining processes are renamed to cover all the gaps in the naming
  from the original communicator (API). See :ref:`MPIX_Comm_shrink`,
  :ref:`MPIX_Comm_ishrink`.
* ``MPIX_Comm_agree(MPI_Comm comm, int *flag)`` performs a consensus
  (i.e. fault tolerant allreduce operation) on flag (with the
  operation bitwise AND) (API).  Absorbs all new failures, and
  propagate the knowledge about failures among the participants. see
  :ref:`MPIX_Comm_agree`, :ref:`MPIX_Comm_iagree`.
* ``MPIX_Comm_get_failed(MPI_Comm comm, MPI_Group* failedgrp)`` obtains the
  group of currently failed processes (API). See :ref:`MPIX_Comm_get_failed`.
* ``MPIX_Comm_ack_failed(MPI_Comm comm, int num_to_ack, int* num_acked)``
  acknowledges that the application intends to ignore the effect of currently
  known failures on wildcard receive completions and agreement return values
  (API). See :ref:`MPIX_Comm_ack_failed`.

Supported Systems
-----------------

There are several MPI communication engines available in Open MPI,
notably:

* PML: ``ob1``, ``cm``, ``ucx``
* MTL: ``ofi``, ``portals4``, ``psm2``

However, in Open MPI |ompi_ver|, only ``ob1`` is fully adapted to support
fault tolerance. The UCX PML has been successfully tested in some setups,
but at this point we cannot confirm that all UCT devices are fully capable
to provide the necessary features.

``ob1`` uses BTL ("Byte Transfer Layer") components for each supported
network. ``ob1`` supports a variety of networks that can be used in
combination with each other. Collective operations (blocking and
non-blocking) use an optimized implementation on top of  ``ob1``.

- Loopback (send-to-self)
- TCP
- UCT (InfiniBand)
- uGNI (Cray Gemini, Aries)
- Shared Memory (FT supported with CMA and XPMEM; KNEM is untested)
- Tuned and non-blocking collective communications

A full list of supported, untested and disabled components is provided
later in this document.

ULFM web site
-------------

More information (tutorials, examples, build instructions for leading
top500 systems) is also available in the Fault Tolerance Research
Hub website: https://fault-tolerance.org

Bibliographic References
------------------------

If you are looking for, or want to cite a general reference for ULFM,
please use:

    *Wesley Bland, Aurelien Bouteiller, Thomas Herault, George Bosilca, Jack
    J. Dongarra: Post-failure recovery of MPI communication
    capability: Design and rationale. IJHPCA 27(3): 244-254 (2013).*

Available from DOI :doi:`10.1177/1094342013488238`.

Building ULFM support in Open MPI
---------------------------------

In Open MPI |ompi_ver|, ULFM support is **built-in by default** |mdash|
that is, when you build Open MPI, unless you specify ``--without-ft``, ULFM
support is automatically available (but is inactive unless enabled at
runtime).

Optionally, you can specify ``--with-ft ulfm`` to ensure that ULFM support
is definitely built.

.. note:: ULFM Fault Tolerance does not apply to OpenSHMEM.  It is recommended
   that if you are going to use ULFM, you should disable building OpenSHMEM
   with ``--disable-oshmem``.

Running ULFM Open MPI
---------------------

Building your application
^^^^^^^^^^^^^^^^^^^^^^^^^

As ULFM is still an extension to the MPI standard, you will need to
``#include <mpi-ext.h>`` in C, or ``use mpi_ext`` in Fortran to access
the supplementary error codes and functions.

Compile your application as usual, using the provided ``mpicc`` or
``mpifort`` wrappers.

Running your application
^^^^^^^^^^^^^^^^^^^^^^^^

You can launch your application with fault tolerance by simply using
the normal Open MPI ``mpirun`` launcher, with the
``--with-ft ulfm`` CLI option (or its synonym ``--with-ft mpi``):

.. code-block::

   shell$ mpirun --with-ft ulfm ...

.. important:: By default, fault tolerance is not active at run time.
   It must be enabled via ``--with-ft ulfm``.

Running under a batch scheduler
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

ULFM can operate under a job/batch scheduler, and is tested routinely
with ALPS, PBS, and Slurm. One difficulty comes from the fact that
many job schedulers handle failures by triggering an immediate "cleanup"
of the application as soon as any process fails. In addition, failure
detection subsystems integrated into PRTE are not active in direct launch
scenarios and may not have a launcher specific alternative. This may cause
the application to not detect failures and lock. In order to avoid these
problems, it is preferred that you use ``mpirun`` within an allocation
(e.g., ``salloc``, ``sbatch``, ``qsub``) rather than a direct launch.

* SLURM is tested and supported with fault tolerance.

  .. important:: Use ``mpirun`` in an ``salloc`` or ``sbatch`` allocation.
     Direct launch with ``srun`` is not supported.

* PBS/Torque is tested and supported with fault tolerance.

  .. important:: Use ``mpirun`` in a ``qsub`` allocation. Direct launch
     with ``aprun`` is not supported.

* LSF is untested with fault tolerance.

Run-time tuning knobs
^^^^^^^^^^^^^^^^^^^^^

ULFM comes with a variety of knobs for controlling how it runs. The
default parameters are sane and should result in good performance in
most cases. You can change the default settings with ``--mca
mpi_ft_foo <value>`` for Open MPI options, and with ``--prtemca
errmgr_detector_bar <value>`` for PRTE options.

.. important:: The main control for enabling/disabling fault tolerance
   at runtime is the ``--with-ft ulfm`` (or its synomym ``--with-ft mpi``)
   ``mpirun`` CLI option. This option sets up multiple subsystems in
   Open MPI to enable fault tolerance. The options described below are
   best used to override the default behavior after the ``--with-ft ulfm``
   opion is used.

PRTE level options
~~~~~~~~~~~~~~~~~~

* ``prrte_enable_ft <true|false> (default: false)`` controls
  automatic cleanup of apps with failed processes within
  ``mpirun``. This option is automatically set to ``true`` when using
  ``--with-ft ulfm``.
* ``errmgr_detector_priority <int> (default 1005``) selects the
  PRRTE-based failure detector. Only available when
  ``prte_enable_recovery`` is ``true``. You can set this to ``0`` when
  using the (experimental) Open MPI detector instead.
* ``errmgr_detector_heartbeat_period <float> (default: 5e0)`` controls
  the heartbeat period. Recommended value is 1/2 of the timeout.
* ``errmgr_detector_heartbeat_timeout <float> (default: 1e1 seconds)``
  heartbeat timeout (i.e. failure detection speed). Recommended value
  is 2 times the heartbeat period. The default setup is tuned for
  failure-free performance at the expense of fault detection
  reactivity. In environments where faults are expected to be common,
  less conservative values can be used (e.g., 100ms); Values lower
  than the TCP poll rate (typically 10ms) can cause false positive.

Open MPI level options
~~~~~~~~~~~~~~~~~~~~~~

Default values are applied to some Open MPI parameters when using
``mpirun --with-ft ulfm``. These defaults are obtained from the ``ft-mpi``
aggregate MCA param file
``$installdir/share/openmpi/amca-param-sets/ft-mpi``. You can tune the
runtime behavior of ULFM by either setting or unsetting variables in
this file, or by overriding the variable on the command line (e.g.,
``--mca btl ofi,self``).

.. important:: Note that if fault tolerance is disabled at runtime,
   (that is, when not using ``--with-ft ulfm``), the ``ft-mpi`` AMCA
   param file is not loaded, thus components that are unsafe for fault
   tolerance will load normally (this may change observed performance
   when comparing with and without fault tolerance).

* ``mpi_ft_enable <true|false> (default: false)``
  permits turning on/off fault tolerance at runtime. This option is
  automatically set to ``true`` from the aggregate MCA param file
  ``ft-mpi`` loaded when using ``--with-ft ulfm``. When false, failure
  detection is disabled; Interfaces defined by the fault tolerance extensions
  are substituted with dummy non-fault tolerant implementations (e.g.,
  ``MPIX_Comm_agree`` is implemented with ``MPI_Allreduce``); All other
  controls below become irrelevant.
* ``mpi_ft_verbose <int> (default: 0)`` increases the output of the
  fault tolerance activities. A value of 1 will report detected
  failures.
* ``mpi_ft_detector <true|false> (default: false)``, **DEPRECATED**
  controls the activation of the Open MPI level failure detector. When
  this detector is turned off, all failure detection is delegated to
  PRTE (see above).  The Open MPI level fault detector is
  experimental. There is a tradeoff between failure detection accuracy
  and performance with this detector. Users that experience accuracy
  issues may enable a more precise mode.  See the tuning knobs below
  to adjust to taste; The Open MPI failure detector operates on
  ``MPI_COMM_WORLD`` exclusively.  Processes connected from
  ``MPI_COMM_CONNECT``/``ACCEPT`` and ``MPI_COMM_SPAWN`` may
  occasionally not be detected when they fail.

  .. caution:: This component is deprecated. Failure detection is now
     performed at the PRTE level. See the section above on controlling
     PRTE behavior for information about how to tune the failure detector.

* ``mpi_ft_detector_thread <true|false> (default: false)`` controls
  the use of a thread to emit and receive failure detector's
  heartbeats. *Setting this value to "true" will also set
  MPI_THREAD_MULTIPLE support, which has a noticeable effect on
  latency (typically 1us increase).* You may want to **enable this
  option if you experience false positive** processes incorrectly
  reported as failed with the Open MPI failure detector.

  .. important:: This option is only relevant when ``mpi_ft_detector`` is ``true``.

* ``mpi_ft_detector_period <float> (default: 3e0 seconds)`` heartbeat
  period. Recommended value is 1/3 of the timeout. _Values lower than
  100us may impart a noticeable effect on latency (typically a 3us
  increase)._

  .. important:: This option is only relevant when ``mpi_ft_detector`` is ``true``.

* ``mpi_ft_detector_timeout <float> (default: 1e1 seconds)`` heartbeat
  timeout (i.e. failure detection speed). Recommended value is 3 times
  the heartbeat period.

  .. important:: This option is only relevant when ``mpi_ft_detector`` is ``true``.

Known Limitations in ULFM
-------------------------

* InfiniBand support is provided through the UCT BTL; fault tolerant
  operation over the UCX PML is not yet supported for production runs.
* TOPO, FILE, RMA are not fault tolerant. They are expected to work
  properly before the occurrence of the first failure.

Modified, Untested and Disabled Components
------------------------------------------

Frameworks and components are listed below and categorized into one of
three classifications:

1. **Modified:** This framework/component has been specifically modified
   such that it will continue to work after a failure.
2. **Untested:** This framework/component has not been modified and/or
   tested with fault tolerance scenarios, and _may_ malfunction
   after a failure.
3. **Disabled:** This framework/component will cause unspecified behavior when
   fault tolerance is enabled. As a consequence, it will be disabled when the
   ``--with-ft ulfm`` option is used (see above for defails about implicit
   parameters loaded from the ``ft-mpi`` aggregate param file).

Any framework or component not listed below are categorized as **Unmodified**,
meaning that it is unmodified for fault tolerance, but will continue to work
correctly after a failure.

* ``pml``: MPI point-to-point management layer

  * ``monitoring``, ``v``: **untested** (they have not been modified to handle
    faults)
  * ``cm``, ``crcpw``, ``ucx``: **disabled**

* ``btl``: Point-to-point Byte Transfer Layer

  * ``ofi``, ``portals4``, ``smcuda``, ``usnic``, ``sm(+knem)``: **untested**
    (they may work properly, please report)

* ``mtl``: Matching transport layer Used for MPI point-to-point messages on
  some types of networks

  * All ``mtl`` components are **disabled**

* ``coll``: MPI collective algorithms

  * ``cuda``, ``inter``, ``sync``, ``sm``: **untested** (they have not
    been modified to handle faults, but we expect correct post-fault
    behavior)
  * ``hcoll``, ``portals4`` **disabled** (they have not been modified
    to handle faults, and we expect unspecified post-fault behavior)

* ``osc``: MPI one-sided communications

  * All ``osc`` components are **untested** (they have not been
    modified to handle faults, and we expect unspecified post-fault
    behavior)

* ``io``: MPI I/O and dependent components

  * ``fs``: File system functions for MPI I/O
  * ``fbtl``: File byte transfer layer: abstraction for individual
    read/write operations for OMPIO
  * ``fcoll``: Collective read and write operations for MPI I/O
  * ``sharedfp``: Shared file pointer operations for MPI I/O
  * All components in these frameworks are unmodified, **untested**
    (we expect clean post-failure abort)

* ``vprotocol``: Checkpoint/Restart components

  * All ``vprotocol`` components are **untested**

* ``threads``, ``wait-sync``: Multithreaded wait-synchronization
  object

  * ``argotbots``, ``qthreads``: **disabled** (these components have
    not been modified to handle faults; we expect post-failure
    deadlock)

Changelog
---------

ULFM Integrated in Open MPI
^^^^^^^^^^^^^^^^^^^^^^^^^^^

As of |ompi_ver|, ULFM is now integrated directly in to the community
release of Open MPI.  The following sections describe previous ULFM
standlone releases.

ULFM Standalone Release 4.0.2u1
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is a stability and upstream parity upgrade. It is based on the
most current Open MPI Release (v4.0.2, October 2019).

* This release is based on Open MPI release v4.0.2 (ompi #cb5f4e737a).
* This release is based on ULFM master (ulfm #0e249ca1).
* New features

  * Support for the UCT BTL enters beta stage.

* Bugfixes

  * High sensitivity to noise in the failure detector.
  * Deadlocks when revoking while BTL progress threads are updating messages.
  * A case where the failure detector would keep observing a dead
    process forever.
  * Disable the use of external pmix/libevent by default (the
    internals are modified to handle error cases).
  * Clean error paths leaving some rdma registration dangling.
  * Do not remove the orte job/proc session dir prematurely upon
    error.

ULFM Standalone Release 4.0.1u1
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is a stability and upstream parity upgrade. It improves
stability, performance and is based on the most current Open MPI
Release (v4.0.1, May 2019).

* This release is based on Open MPI release v4.0.1 (ompi #b780667).
* This release is based on ULFM master (ulfm #cf8dc43f).
* New features

  * Addition of the ``MPI_Comm_is_revoked`` function
  * Renamed ``ftbasic`` collective component to ``ftagree``
  * Restored the ``pcollreq`` extension

* Bugfixes

  * Failures of node-local siblings were not always detected
  * Failure propagation and detection was slowed down by trying to
    notify known dead processes
  * There were deadlocks in multithreaded programs
  * There were issues with PMPI when compiling Fortran Interfaces
  * There were deadlocks on OS-X

ULFM Standalone Release 2.1
^^^^^^^^^^^^^^^^^^^^^^^^^^^

This release is a bugfix and upstream parity upgrade. It improves
stability, performance and is based on the most current Open MPI
main (November 2018).

* ULFM is now based upon Open MPI main branch (#37954b5f).
* ULFM tuning MCA parameters are exposed by ``ompi_info``.
* Fortran 90 bindings have been updated
* Bugfixes:

  * Correct the behavior of process placement during an MPI_COMM_SPAWN
    when some slots were occcupied by failed processes.
  * MPI_COMM_SPAWN accepts process placement directives in the Info object.
  * Fixed deadlocks in some NBC collective operations.
  * Crashes and deadlocks in MPI_FINALIZE have been resolved.
  * Any-source requests that returned with an error status of
    MPIX_PROC_FAILED_PENDING can now correctly complete during later
    MPI_WAIT/TEST.

ULFM Standalone Release 2.0
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Focus has been toward integration with current Open MPI main
(November 2017), performance, and stability.

* ULFM is now based upon Open MPI main branch (#689f1be9). It will
  be regularly updated until it will eventually be merged.
* Fault Tolerance is enabled by default and is controlled with MCA variables.
* Added support for multithreaded modes (MPI_THREAD_MULTIPLE, etc.)
* Added support for non-blocking collective operations (NBC).
* Added support for CMA shared memory transport (Vader).
* Added support for advanced failure detection at the MPI level.
  Implements the algorithm described in "Failure detection and
  propagation in HPC systems," DOI :doi:`10.1109/SC.2016.26`.
* Removed the need for special handling of CID allocation.
* Non-usable components are automatically removed from the build
  during configure
* RMA, FILES, and TOPO components are enabled by default, and usage in
  a fault tolerant execution warns that they may cause undefined
  behavior after a failure.
* Bugfixes:

  * Code cleanup and performance cleanup in non-FT builds; --without-ft at
    configure time gives an almost stock Open MPI.
  * Code cleanup and performance cleanup in FT builds with FT runtime disabled;
    --mca ft_enable_mpi false thoroughly disables FT runtime activities.
  * Some error cases would return ERR_PENDING instead of ERR_PROC_FAILED in
    collective operations.
  * Some test could set ERR_PENDING or ERR_PROC_FAILED instead of
    ERR_PROC_FAILED_PENDING for ANY_SOURCE receptions.

ULFM Standalone Release 1.1
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Focus has been toward improving stability, feature coverage for
intercomms, and following the updated specification for
MPI_ERR_PROC_FAILED_PENDING.

* Forked from Open MPI 1.5.5 devel branch
* Addition of the MPI_ERR_PROC_FAILED_PENDING error code, as per newer
  specification revision. Properly returned from point-to-point,
  non-blocking ANY_SOURCE operations.
* Alias MPI_ERR_PROC_FAILED, MPI_ERR_PROC_FAILED_PENDING and
  MPI_ERR_REVOKED to the corresponding standard blessed -extension-
  names MPIX_ERR_xxx.
* Support for Intercommunicators:

  * Support for the blocking version of the agreement, MPI_COMM_AGREE
    on Intercommunicators.
  * MPI_COMM_REVOKE tested on intercommunicators.

* Disabled completely (.ompi_ignore) many untested components.
* Changed the default ORTE failure notification propagation
  aggregation delay from 1s to 25ms.
* Added an Open MPI internal failure propagator; failure propagation
  between SM domains is now immediate.
* Bugfixes:

  * SendRecv would not always report MPI_ERR_PROC_FAILED correctly.
  * SendRecv could incorrectly update the status with errors
    pertaining to the Send portion of the Sendrecv.
  * Revoked send operations are now always completed or remote
    cancelled and may not deadlock anymore.
  * Cancelled send operations to a dead peer will not trigger an
    assert when the BTL reports that same failure.
  * Repeat calls to operations returning MPI_ERR_PROC_FAILED will
    eventually return MPI_ERR_REVOKED when another process revokes the
    communicator.

ULFM Standalone Release 1.0
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Focus has been toward improving performance, both before and after the
occurrence of failures.  The list of new features includes:

* Support for the non-blocking version of the agreement, MPI_COMM_IAGREE.
* Compliance with the latest ULFM specification draft. In particular,
  the MPI_COMM_(I)AGREE semantic has changed.
* New algorithm to perform agreements, with a truly logarithmic
  complexity in number of ranks, which translates into huge
  performance boosts in MPI_COMM_(I)AGREE and MPI_COMM_SHRINK.
* New algorithm to perform communicator revocation. MPI_COMM_REVOKE
  performs a reliable broadcast with a fixed maximum output degree,
  which scales logarithmically with the number of ranks.
* Improved support for our traditional network layer:

  * TCP: fully tested
  * SM: fully tested (with the exception of XPMEM, which remains unsupported)

* Added support for High Performance networks

  * Open IB: reasonably tested
  * uGNI: reasonably tested

* The tuned collective module is now enabled by default (reasonably
  tested), expect a huge performance boost compared to the former
  basic default setting

  * Back-ported PBS/ALPS fixes from Open MPI
  * Back-ported OpenIB bug/performance fixes from Open MPI
  * Improve Context ID allocation algorithm to reduce overheads of
    Shrink
  * Miscellaneous bug fixes

Binary Compatibility
^^^^^^^^^^^^^^^^^^^^

ULFM Open MPI is binary compatible with any version of Open MPI
compatible with the underlying Open MPI main branch or release (see
the binary compatibility and version number section in the upstream
Open MPI README). That is, applications compiled with a compatible
Open MPI can run with the ULFM Open MPI ``mpirun`` and MPI
libraries. Conversely, *as long as the application does not employ one
of the MPIX functions,* which are exclusively defined in ULFM Open
MPI, an application compiled with ULFM Open MPI can be launched with a
compatible Open MPI ``mpirun`` and run with the non-fault tolerant MPI
library.

Contacting the Authors
----------------------

Found a bug?  Got a question?  Want to make a suggestion?  Want to
contribute to ULFM Open MPI?  Working on a cool use-case?
Please let us know!

The best way to report bugs, send comments, or ask questions is to
sign up on the user's mailing list: ulfm+subscribe@googlegroups.com

Because of spam, only subscribers are allowed to post to these lists
(ensure that you subscribe with and post from exactly the same e-mail
address |mdash| joe@example.com is considered different than
joe@mycomputer.example.com!).  Visit these pages to subscribe to the
lists: https://groups.google.com/forum/#!forum/ulfm

When submitting questions and problems, be sure to include as much
extra information as possible.  See the :doc:`Getting help
</getting-help>` section for more details.

Thanks for your time.


ULFM Copyright
--------------

Copyright (c) 2012-|year| The University of Tennessee and The
University of Tennessee Research Foundation.  All rights reserved.
