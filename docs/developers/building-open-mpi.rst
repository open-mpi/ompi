Building Open MPI
=================

General
-------

Once you have run ``autogen.pl`` successfully, you can configure and
build Open MPI just like end users do with official distribution Open
MPI tarballs.

See the :doc:`general "Install Open MPI" documentation for more
details. </installing-open-mpi/index>`

Building Against External OpenPMIx / PRRTE
------------------------------------------

One thing that developers and/or packagers may need to do is to build
Open MPI against an external OpenPMIx installation, and/or configure
Open MPI's ``mpirun`` / ``mpiexec`` launchers to use an external PRRTE
installation (i.e., installations that were not built from the
embedded copies inside the Open MPI source tree / Git submodules).

With regards to :doc:`Open MPI's required dependent libraries
</installing-open-mpi/required-support-libraries>` (Hwloc, Libevent,
OpenPMIx, and PRRTE), it generally is simplest to build Open MPI in
one of two ways:

#. Build and use all the **internal** copies of Open MPI's required
   dependent libraries.

   * Specifically: use the Hwloc, Libevent, OpenPMIx, and PRRTE source
     trees that are bundled in with Open MPI's source code.

#. Build and use all **external** copies of Open MPI's required
   dependent libraries.

   * Specifically: ignore the Hwloc, Libevent, OpenPMIx, and PRRTE source
     trees that are bundled in with Open MPI's source code, and,
     instead, build Open MPI against already-installed Hwloc,
     Libevent, and OpenPMIx libraries, and configure Open MPI's
     launchers to use an already-installed PRRTE.

Other variations are possible, but can get tricky and complicated
because Open MPI, the OpenPMIx library that it uses, Hwloc, and
Libevent can be loaded into the same process.  They are therefore not
recommended unless you understand the run-time linker consequences.

Some facts that are relevant to know when building against an external
OpenPMIx / PRRTE:

1. Open MPI and the OpenPMIx library that Open MPI links against must
   be built against the **same** installation of Hwloc and Libevent.
   Meaning:

   * Assumedly the external OpenPMIx was built against external Hwloc
     and Libevent.  Open MPI **must** compile and link against the
     **same** Hwloc and Libevent that the external OpenPMIx was built
     against.

     .. admonition:: Critical
        :class: Danger

        Open MPI and the OpenPMIx library that it links against must
        use the same Hwloc and Libevent libraries at run time (e.g.,
        they must resolve to the same run-time loadable libraries at
        run time).

        .. important:: This statement applies regardless of whether
                       Open MPI -- and/or the other libraries -- are
                       built as static or dynamically-loadable
                       libraries.

   * Unless you really know what you are doing, this usually means
     building and installing Open MPI against the same installation
     tree(s) of Hwloc and Libevent that OpenPMIx used to build itself.

     For example, consider an environment where you install Hwloc,
     Libevent, OpenPMIx, and PRRTE via the operating system's package
     manager.  Assuming that the package-manager install of OpenPMIx
     was built against the package-manager-provided Hwloc and
     Libevent, then Open MPI will *also* need to be built against the
     package-manager-provided Hwloc and Libevent.  To build Open MPI
     this way, you may need to install the package manager's
     "developer" Hwloc, Libevent, and OpenPMIx packages.

1. PRRTE and the OpenPMIx library that PRRTE uses must be built
   against the **same** installation of Hwloc and Libevent.

   This is a separate requirement from Open MPI's requirement above:
   Open MPI does not link against PRRTE, and MPI applications do not
   load ``libprrte``.  Therefore, PRRTE's Hwloc, Libevent, and
   OpenPMIx dependencies do not have to match Open MPI's dependencies
   merely because Open MPI uses PRRTE as a launcher.

1. Open MPI and PRRTE do **not** have to use the same OpenPMIx
   installation.

   PMIx supports cross-version operations, so Open MPI and PRRTE can
   use different OpenPMIx installations, and those installations do
   not need to be the same OpenPMIx version.

   If Open MPI and PRRTE do use the **same** OpenPMIx installation,
   then the requirements above mean that Open MPI, PRRTE, OpenPMIx,
   Hwloc, and Libevent will all use the same Hwloc and Libevent
   installations.  However, this is a consequence of sharing one
   OpenPMIx installation; it is not a requirement that Open MPI and
   PRRTE share one OpenPMIx installation.
