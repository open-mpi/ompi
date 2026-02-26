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
Open MPI against an external OpenPMIx and/or PRRTE source tree (i.e.,
an OpenPMIx and/or PRRTE installation that was not built from the
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
     instead, compile and link Open MPI against already-installed
     versions of these libraries.

Other variations are possible, but can get tricky and complicated with
subtle linker consequences, and are therefore not recommended.

Some facts that are relevant to know when building against an external
OpenPMIx / PRRTE:

1. Open MPI, OpenPMIx, and PRRTE must all be built against the
   **same** installation of Hwloc and Libevent.  Meaning:

   * Assumedly the external OpenPMIx and PRRTE were built against
     external Hwloc and Libevent.  Open MPI **must** compile and link
     against the **same** Hwloc and Libevent that the external
     OpenPMIx and PRRTE were built against.

     .. admonition:: Critical
        :class: Danger

        Open MPI, OpenPMIx, and PRRTE must all use the same Hwloc and
        Libevent libraries at run time (e.g., they must all resolve to
        the same run-time loadable libraries at run time).

        .. important:: This statement applies regardless of whether
                       Open MPI -- and/or the other libraries -- are
                       built as static or dynamically-loadable
                       libraries.

   * Unless you really know what you are doing, this usually means
     building and installing Open MPI against the same installation
     tree(s) of Hwloc and Libevent that OpenPMIx and PRRTE used to
     build themselves.

     For example, consider an environment where you install Hwloc,
     Libevent, OpenPMIx, and PRRTE via the operating system's package
     manager.  Assuming that the package-manager installs of OpenPMIx
     and PRRTE were built against the package-manager-provider Hwloc
     and Libevent, then Open MPI will *also* need to be built against
     the package-manager-provided Hwloc and Libevent.  To build Open
     MPI this way, you may need to install the package manager's
     "developer" Hwloc, Libevent, OpenPMIx, and/or PRRTE packages.

1. Open MPI and PRRTE must be built against the **same** installation
   of OpenPMIx.

   .. important:: Similar to how OpenPMIx, PRRTE, and Open MPI, must
                  be built against the same Hwloc and Libevent, PRRTE
                  and Open MPI must be built against the same
                  OpenPMIx.
