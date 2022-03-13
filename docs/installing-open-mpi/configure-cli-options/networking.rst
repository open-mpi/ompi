.. This file is included by building-open-mpi.rst

.. _install-network-support-label:

Networking support / options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following are command line options for various network types that
can be used with ``configure``:

* ``--with-fca=DIR``:
  Specify the directory where the Mellanox FCA library and
  header files are located.

  FCA is the support library for Mellanox switches and HCAs.

* ``--with-hcoll=DIR``:
  Specify the directory where the Mellanox hcoll library and header
  files are located.  This option is generally only necessary if the
  hcoll headers and libraries are not in default compiler/linker
  search paths.

  hcoll is the support library for MPI collective operation offload on
  Mellanox ConnectX-3 HCAs (and later).

* ``--with-knem=DIR``:
  Specify the directory where the knem libraries and header files are
  located.  This option is generally only necessary if the knem headers
  and libraries are not in default compiler/linker search paths.

  knem is a Linux kernel module that allows direct process-to-process
  memory copies (optionally using hardware offload), potentially
  increasing bandwidth for large messages sent between messages on the
  same server.  See `the Knem web site
  <https://knem.gitlabpages.inria.fr/>`_ for details.

* ``--with-libfabric=DIR``:
  Specify the directory where the OpenFabrics Interfaces ``libfabric``
  library and header files are located.  This option is generally only
  necessary if the libfabric headers and libraries are not in default
  compiler/linker search paths.

  Libfabric is the support library for OpenFabrics Interfaces-based
  network adapters, such as Cisco usNIC, Intel True Scale PSM, Cray
  uGNI, etc.

* ``--with-libfabric-libdir=DIR``:
  Look in directory for the libfabric libraries.  By default, Open MPI
  will look in ``DIR/lib`` and ``DIR/lib64``, which covers most cases.
  This option is only needed for special configurations.

* ``--with-portals4=DIR``:
  Specify the directory where the Portals4 libraries and header files
  are located.  This option is generally only necessary if the Portals4
  headers and libraries are not in default compiler/linker search
  paths.

  Portals is a low-level network API for high-performance networking
  on high-performance computing systems developed by Sandia National
  Laboratories, Intel Corporation, and the University of New Mexico.
  The Portals 4 Reference Implementation is a complete implementation
  of Portals 4, with transport over InfiniBand verbs and UDP.

* ``--with-portals4-libdir=DIR``:
  Location of libraries to link with for Portals4 support.

* ``--with-portals4-max-md-size=SIZE`` and
  ``--with-portals4-max-va-size=SIZE``:
  Set configuration values for Portals 4

* ``--with-psm=<directory>``:
  Specify the directory where the QLogic InfiniPath / Intel True Scale
  PSM library and header files are located.  This option is generally
  only necessary if the PSM headers and libraries are not in default
  compiler/linker search paths.

  PSM is the support library for QLogic InfiniPath and Intel TrueScale
  network adapters.

* ``--with-psm-libdir=DIR``:
  Look in directory for the PSM libraries.  By default, Open MPI will
  look in ``DIR/lib`` and ``DIR/lib64``, which covers most cases.  This
  option is only needed for special configurations.

* ``--with-psm2=DIR``:
  Specify the directory where the Intel Omni-Path PSM2 library and
  header files are located.  This option is generally only necessary
  if the PSM2 headers and libraries are not in default compiler/linker
  search paths.

  PSM is the support library for Intel Omni-Path network adapters.

* ``--with-psm2-libdir=DIR``:
  Look in directory for the PSM2 libraries.  By default, Open MPI will
  look in ``DIR/lib`` and ``DIR/lib64``, which covers most cases.  This
  option is only needed for special configurations.

* ``--with-ucx=DIR``:
  Specify the directory where the UCX libraries and header files are
  located.  This option is generally only necessary if the UCX headers
  and libraries are not in default compiler/linker search paths.

* ``--with-ucx-libdir=DIR``:
  Look in directory for the UCX libraries.  By default, Open MPI will
  look in ``DIR/lib`` and ``DIR/lib64``, which covers most cases.  This
  option is only needed for special configurations.

* ``--with-usnic``:
  Abort configure if Cisco usNIC support cannot be built.
