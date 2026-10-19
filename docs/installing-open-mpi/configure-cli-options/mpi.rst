.. This file is included by building-open-mpi.rst

MPI functionality
^^^^^^^^^^^^^^^^^

The following are command line options to set the default for various
MPI API behaviors that can be used with ``configure``:

* ``--with-mpi-param-check[=VALUE]``:
  Whether or not to check MPI function parameters for errors at
  runtime.  The following ``VALUE``\s are permitted:

  * ``always``: MPI function parameters are always checked for errors
  * ``never``: MPI function parameters are never checked for errors
  * ``runtime``: Whether MPI function parameters are checked depends on
    the value of the MCA parameter ``mpi_param_check`` (default: yes).
  * ``yes``: Synonym for "always" (same as ``--with-mpi-param-check``).
  * ``no``: Synonym for "never" (same as ``--without-mpi-param-check``).

  If ``--with-mpi-param`` is not specified, ``runtime`` is the default.

* ``--disable-mpi-thread-multiple``:
  Disable the MPI thread level ``MPI_THREAD_MULTIPLE`` (it is enabled by
  default).

* ``--disable-ft``:
  Disable the User-Level Fault Mitigation (ULFM) support in Open MPI
  (it is enabled by default).

  :ref:`See the ULFM section <ulfm-label>` for more information.

* ``--enable-mpi-fortran[=VALUE]``:
  By default, Open MPI will attempt to build all 3 Fortran bindings:
  ``mpif.h``, the ``mpi`` module, and the ``mpi_f08`` module.  The following
  ``VALUE``\s are permitted:

  * ``all``: Synonym for ``yes``.
  * ``yes``: Attempt to build all 3 Fortran bindings; skip
    any binding that cannot be built (same as
    ``--enable-mpi-fortran``).
  * ``mpifh``: Only build ``mpif.h`` support.
  * ``usempi``: Only build ``mpif.h`` and ``mpi`` module support.
  * ``usempif08``:  Build ``mpif.h``, ``mpi`` module, and ``mpi_f08``
    module support.
  * ``none``: Synonym for ``no``.
  * ``no``: Do not build any MPI Fortran support (same as
    ``--disable-mpi-fortran``).  This is mutually exclusive
    with building the OpenSHMEM Fortran interface.

* ``--with-mpi-moduledir=DIR``:
  Specify a specific ``DIR`` directory where to install the MPI
  Fortran bindings modulefiles.  By default, Open MPI will install
  Fortran modulefiles into ``$libdir``.

* ``--enable-mpi-abi[=MODE]``:
  Select which MPI ABIs Open MPI builds.  ``MODE`` is one of:

  * ``both`` (the default): build both the traditional Open MPI ABI
    (``libmpi``) and the MPI Forum standard ABI (``libmpi_abi``),
    allowing applications to use either.  A bare ``--enable-mpi-abi``
    (with no ``=MODE``) also selects ``both``.
  * ``standard`` (or ``forum``): build **only** the MPI Forum standard
    ABI.  This is "ABI-only" mode (see the limitations below).
  * ``ompi``: build **only** the traditional Open MPI ABI; no
    ``libmpi_abi`` is produced.

  When the MPI Forum standard ABI is built, Open MPI installs
  ``libmpi_abi``, the ``mpicc_abi`` convenience wrapper, and
  ``ompi-forum-abi*`` pkg-config files.

  .. warning:: **Limitations of ABI-only mode**
               (``--enable-mpi-abi=standard``):

               * **No Fortran bindings**: The MPI Forum standard
                 currently only defines a C ABI. No Fortran compiler
                 wrappers (``mpifort``, ``mpif77``, ``mpif90``) or
                 bindings are available.
               * **No OpenSHMEM support**: OpenSHMEM requires the
                 traditional Open MPI ABI and is disabled in
                 ABI-only mode.

  .. note:: **Deprecated synonyms.**  The older boolean options are
            still accepted (with a deprecation warning) but should be
            replaced with ``--enable-mpi-abi=MODE``:

            * ``--enable-standard-abi`` / ``--disable-standard-abi`` ---
              formerly toggled the MPI Forum standard ABI.
            * ``--disable-ompi-abi`` --- equivalent to
              ``--enable-mpi-abi=standard``; required the standard ABI
              to be enabled.

* ``--enable-mpi-ext[=LIST]``:
  Enable Open MPI's non-portable API extensions.  ``LIST`` is a
  comma-delimited list of extensions.  If no ``LIST`` is specified, all
  of the extensions are enabled.

  See the "Open MPI API Extensions" section for more details.

* ``--disable-mpi-io``:
  Disable built-in support for MPI-2 I/O, likely because an
  externally-provided MPI I/O package will be used.

* ``--disable-io-ompio``:
  Disable the ompio MPI-IO component

* ``--enable-sparse-groups``:
  Enable the usage of sparse groups. This would save memory
  significantly especially if you are creating large
  communicators. (Disabled by default)
