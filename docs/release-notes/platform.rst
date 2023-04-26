.. _platform-notes-section-label:

Platform Notes
==============

.. error:: **TODO We should have a canonical list of:**

   *  *required* 3rd-party package versions supported (PRRTE, hwloc,
      libevent)
   * back-end run-time systems supported (behind PRRTE)
   * OS's and compilers supported
   * network interconnects supported.

Open MPI uses both `OpenPMIx <https://openpmix.github.io/>`_ and
`PRRTE <https://github.com/openpmix/prrte>`_ for its run-time system support,
and therefore supports whatever run-time systems they support.

.. note:: See the :ref:`PMIx and PRRTE section
          <label-running-role-of-pmix-and-prte>` of the Open MPI
          documentation for more details about those projects and
          their relationship to Open MPI.

Each version of Open MPI supports a specific set of versions of
OpenPMIx and PRRTE.  Those versions therefore determine which run-time systems
that a release of Open MPI supports.

* Systems that have been tested are:

  * Linux (various flavors/distros), 64 bit (x86, ppc, aarch64),
    with gcc (>=4.8.x+), clang (>=3.6.0), Absoft (fortran), Intel,
    and Portland (be sure to also see :ref:`the Compiler Notes
    section <compiler-notes-section-label>`)
  * macOS (10.14-10.15, 11.x, 12.x), 64 bit (x86_64) with XCode
    compilers

* Other systems have been lightly (but not fully) tested:

  * Cygwin 64 bit with gcc
  * ARMv6, ARMv7, ARMv9
  * Other 64 bit platforms.
  * OpenBSD.  Requires configure options ``--enable-mca-no-build=patcher``
    and ``--disable-dlopen`` with this release.
  * Problems have been reported when building Open MPI on FreeBSD 11.1
    using the clang-4.0 system compiler. A workaround is to build
    Open MPI using the GNU compiler.

.. note:: 32-bit environments are no longer supported.

* The run-time systems that are currently supported are:

  * ssh / rsh
  * PBS Pro, Torque
  * Platform LSF (tested with v9.1.1 and later)
  * Slurm
  * Cray XE, XC, and XK
  * Oracle Grid Engine (OGE) 6.1, 6.2 and open source Grid Engine
