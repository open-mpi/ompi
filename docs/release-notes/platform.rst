.. _platform-notes-section-label:

Platform Notes
==============

Open MPI uses both `OpenPMIx <https://openpmix.github.io/>`_ and
`PRRTE <https://github.com/openpmix/prrte>`_ for its run-time system
support, and therefore supports whatever run-time systems they support.

.. note:: See the :ref:`PMIx and PRRTE section
          <label-running-role-of-pmix-and-prte>` of the Open MPI
          documentation for more details about those projects and
          their relationship to Open MPI.

Required support libraries
--------------------------

Open MPI |ompi_ver| depends on the following support libraries.  Each
may be supplied by a system installation (if it is recent enough) or
used from the copy embedded in the Open MPI distribution:

* `OpenPMIx <https://openpmix.github.io/>`_: version
  |pmix_min_version| or later when building without PRRTE (when
  building with PRRTE, PRRTE's own OpenPMIx minimum applies).  Open
  MPI |ompi_ver| embeds OpenPMIx |pmix_embedded_version|.
* `PRRTE <https://github.com/openpmix/prrte>`_: version
  |prte_min_version| or later.  Open MPI |ompi_ver| embeds PRRTE
  |prte_embedded_version|.  PRRTE is what provides ``mpirun``; it is
  not needed if MPI jobs are always launched directly by a
  PMIx-enabled resource manager.
* `hwloc <https://www.open-mpi.org/projects/hwloc/>`_: version
  |hwloc_min_version| or later, and older than v3.0.0 (Open MPI does
  not yet support the hwloc v3.x series).  Open MPI |ompi_ver| embeds
  hwloc |hwloc_embedded_version|.
* `Libevent <https://libevent.org/>`_: version |event_min_version| or
  later.  Open MPI |ompi_ver| embeds Libevent
  |event_embedded_version|.

See the :ref:`Required support libraries section
<label-install-required-support-libraries>` for the full details,
including how ``configure`` chooses between an embedded and an
external copy of each library.

The versions of OpenPMIx and PRRTE that a given Open MPI release
supports determine which back-end run-time systems that release
supports.

Building Open MPI additionally requires Perl 5 and Python
|python_min_version| or later.  These are needed only to *build* Open
MPI itself |mdash| not to build or run MPI applications.

Operating systems and compilers
-------------------------------

Open MPI |ompi_ver| requires a C11 (or newer) C compiler.

* Systems that have been tested are:

  * Linux (various flavors/distros), 64 bit (x86, ppc, aarch64), with
    gcc/gfortran (>= 7.x), clang (>= 10.x), Intel, and NVIDIA (be sure
    to also see :ref:`the Compiler Notes section
    <compiler-notes-section-label>`)
  * macOS (26.x), 64 bit (arm64/Apple silicon), with Xcode compilers.
    This is the macOS platform exercised by Open MPI's GitHub Actions
    CI, which builds, runs ``make check``, and runs the example
    programs on the ``macos-latest`` runner image.

* Other systems have been lightly (but not fully) tested:

  * macOS on 64 bit x86_64 (Intel) hardware with Xcode compilers.
    Open MPI is expected to work here, but it is not covered by CI.
  * Cygwin 64 bit with gcc
  * ARMv6, ARMv7, ARMv9
  * Other 64 bit platforms.
  * OpenBSD.  Requires configure options
    ``--enable-mca-no-build=patcher`` and ``--disable-dlopen`` with
    this release.
  * Problems have been reported when building Open MPI on FreeBSD 11.1
    using the clang-4.0 system compiler.  A workaround is to build
    Open MPI using the GNU compiler.

.. note:: 32-bit environments are no longer supported.

Run-time systems
----------------

Because Open MPI delegates launch to PRRTE, the supported back-end
run-time systems are those that PRRTE supports.  The currently
supported run-time systems are:

* **ssh / rsh:** used in non-scheduled environments, and also to launch
  within an allocation from a resource manager that does not provide
  its own launch mechanism.  See :doc:`Launching with SSH
  </launching-apps/ssh>`.
* **Slurm:** Open MPI obtains the node list and slot counts from
  Slurm, and uses Slurm's native mechanisms to start processes.
  Slurm's ``srun`` "direct launch" mode is also supported.  See
  :doc:`Launching with Slurm </launching-apps/slurm>`.
* **PBS Pro / Torque / OpenPBS:** the allocation is obtained from the
  resource manager, and processes are started via the TM interface.
  See :doc:`Launching with PBS / Torque </launching-apps/tm>`.
* **Platform LSF** (tested with v9.1.1 and later): the allocation is
  obtained from LSF, and processes are started via LSF's native
  mechanisms.  Problems have been reported with the most recent LSF
  releases; see :doc:`Launching with LSF </launching-apps/lsf>` for
  the details and a workaround.
* **HPE PALS:** processes are started via the Parallel Application
  Launch Service on HPE systems; PALS' ``aprun`` "direct launch" mode
  is also supported.  See :doc:`Launching with HPE PALS
  </launching-apps/pals>`.
* **Grid Engine:** the allocation is obtained from Grid Engine, and
  processes are started via ``qrsh`` so that Grid Engine can monitor
  and control them.  This covers the whole Grid Engine family (Oracle
  Grid Engine, Son of Grid Engine, Altair/Univa Grid Engine, Open
  Cluster Scheduler, and friends); it must be explicitly enabled with
  ``configure --with-sge``.  See :doc:`Launching with Grid Engine
  </launching-apps/gridengine>`.
* **Flux:** the allocation is obtained from Flux, but Flux provides no
  native launch mechanism to PRRTE; processes within the allocation
  are started via ``ssh``.

Network interconnects
---------------------

The primary network transports that Open MPI supports are:

* Shared memory, for on-node communication
* TCP / IP
* InfiniBand and RoCE, via `UCX <https://openucx.org/>`_
* Networks supported by `Libfabric (OFI) <https://libfabric.org/>`_,
  including AWS EFA, Cornelis Networks Omni-Path, and HPE Slingshot
* Cisco usNIC

See the networking pages under :doc:`/tuning-apps/index` for
configuration and tuning details.
