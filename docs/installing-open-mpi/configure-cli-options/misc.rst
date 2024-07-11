.. This file is included by building-open-mpi.rst

Miscellaneous functionality
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following are command line options that don't fit any any of the
above categories that can be used with ``configure``:

* ``--without-memory-manager``:
  Disable building Open MPI's memory manager.  Open MPI's memory
  manager is usually built on Linux based platforms, and is generally
  only used for optimizations with some OpenFabrics-based networks (it
  is not *necessary* for OpenFabrics networks, but some performance
  loss may be observed without it).

  .. warning:: Open MPI's memory management functionality, which provides
               important performance optimizations on OS-bypass networks
               such as InfiniBand, requires the ``dlsym(3)`` interface,
               and therefore does not work with fully-static applications.

* ``--with-ft=TYPE``:
  Specify the type of fault tolerance to enable.  The only allowed
  values are ``ulfm`` and ``none``.  See :ref:`the ULFM section
  <ulfm-label>` for more details.

* ``--enable-peruse``:
  Enable the PERUSE MPI data analysis interface.

* ``--enable-heterogeneous``:
  Enable support for running on heterogeneous clusters (e.g., machines
  with different endian representations).  Heterogeneous support is
  disabled by default because it imposes a minor performance penalty.

  .. danger:: The heterogeneous functionality is currently broken |mdash|
              do not use.

.. _install-wrapper-flags-label:

* ``--with-wrapper-cflags=CFLAGS``
* ``--with-wrapper-cxxflags=CXXFLAGS``
* ``--with-wrapper-fcflags=FCFLAGS``
* ``--with-wrapper-ldflags=LDFLAGS``
* ``--with-wrapper-libs=LIBS``:
  Add the specified flags to the default flags that are used in Open
  MPI's "wrapper" compilers (e.g., ``mpicc`` |mdash| see below for more
  information about Open MPI's wrapper compilers).  By default, Open
  MPI's wrapper compilers use the same compilers used to build Open
  MPI and specify a minimum set of additional flags that are necessary
  to compile/link MPI applications.  These configure options give
  system administrators the ability to embed additional flags in
  OMPI's wrapper compilers (which is a local policy decision).  The
  meanings of the different flags are:

  * ``CFLAGS``: Flags passed by the ``mpicc`` wrapper to the C
    compiler
  * ``CXXFLAGS``: Flags passed by the ``mpic++`` and ``mpiCC``
    wrappers to the C++ compiler
  * ``FCFLAGS``: Flags passed by the ``mpifort`` wrapper to the
    Fortran compiler
  * ``LDFLAGS``: Flags passed by all the wrappers to the linker
  * ``LIBS``: Flags passed by all the wrappers to the linker

  See the section on :ref:`customizing wrapper compiler behavior
  <label-customizing-wrapper-compiler>` to see how to alter the
  wrapper compiler behavior at run time.

* ``--with-mpi-moduledir``: Specify the directory where the Fortran
  MPI module files are installed.

  For historical reasons, Open MPI's Fortran MPI modulefiles are
  installed into ``$libdir`` by default.  This option lets you change
  where they are installed; some users prefer Fortran module files
  installed into ``$installdir``, for example.

  .. note:: If you intend to make your Open MPI installation
            relocatable :ref:`via the OPAL_PREFIX mechanism
            <install-location-opal-prefix>`, you will want to ensure
            to specify the module directory relative to the
            ``prefix``.  For example:

            .. code-block::

               $ ./configure --prefix=/opt/openmpi --with-mpi-moduledir='${includedir}/modules`...

            Note the additional shell quoting that is likely necessary
            to prevent shell variable expansion, and the additional
            ``${}`` around ``includedir`` that is necessary for Open MPI
            to recognize that it is a special name that needs to be
            expanded.

            Finally, note that the Fortran module installation
            directory is *not* one of the :ref:`recognized directories
            that can be specified via environment variable at run time
            <install-location-overriding-individual-directories>`.
            Instead, to make the Fortran module directory relocatable,
            it needs to be relative to one of the other recognized
            directories.
