MPI Functionality and Features
==============================

.. _release-notes-mpi-standard-conformance-label:

MPI Standard conformance
------------------------

In the Open MPI |ompi_series| series, all MPI-|mpi_standard_version|
functionality is supported.  *Some* MPI-4.0 functionality is
supported.

As such, ``MPI_VERSION`` is set to 3 and ``MPI_SUBVERSION`` is set
to 1.

For historical reference:

.. list-table::
   :header-rows: 1

   * - MPI standards conformance
     - Introduced in Open MPI version

   * - MPI-2.0
     - Open MPI v1.2

   * - MPI-2.1
     - Open MPI v1.3

   * - MPI-3.0
     - Open MPI v1.8

   * - MPI-3.1
     - Open MPI v2.0

MPI-4.0 partial compliance
^^^^^^^^^^^^^^^^^^^^^^^^^^

In the Open MPI |ompi_series| series, only partial MPI-4.0 functionality is
supported. This section contains a list of features added for the release.

* Added support for MPI Sessions.
* Added partitioned communication using persistent sends and persistent receives.
* Added persistent collectives to the ``MPI_`` namespace (they were previously
  available via the ``MPIX_`` prefix).
* Added support for :ref:`MPI_Isendrecv()<mpi_isendrecv>` and its variants.
* Added support for :ref:`MPI_Comm_idup_with_info()<mpi_comm_idup_with_info>`.
* Added support for :ref:`MPI_Info_get_string()<mpi_info_get_string>`.
* Added support for ``initial_error_handler`` info key and the
  ``MPI_ERRORS_ABORT`` infrastructure.
* Added support for ``mpi_minimum_alignment`` info key.
* Added support for ``MPI_COMM_TYPE_HW_GUIDED`` and
  ``MPI_COMM_TYPE_HW_UNGUIDED``.
* Added support for :ref:`MPI_Info_create_env()<mpi_info_create_env>`.
* Added error handling for "unbound" errors to ``MPI_COMM_SELF``.
* Added ``MPI_F_STATUS_SIZE``, ``MPI_F_SOURCE``, ``MPI_F_TAG``, and
  ``MPI_F_ERROR``.
* Made :ref:`MPI_Comm_get_info()<mpi_comm_get_info>`,
  :ref:`MPI_File_get_info()<mpi_file_get_info>`, and
  :ref:`MPI_Win_get_info()<mpi_win_get_info>` MPI-4.0 compliant.
* Info keys that are not understood by Open MPI will be silently ignored and
  dropped on communicators, files, and windows.
* Deprecated :ref:`MPI_Sizeof()<mpi_sizeof>`.
* Deprecated :ref:`MPI_Cancel()<mpi_cancel>` on send requests.
* Deprecated :ref:`MPI_Info_get()<mpi_info_get>` and
  :ref:`MPI_Info_get_valuelen()<mpi_info_get_valuelen>`.

Removed MPI APIs
----------------

Note that starting with Open MPI v4.0.0, prototypes for several
legacy MPI-1 symbols that were deleted in the MPI-3.0 specification
are no longer available by default in ``mpi.h``.  Specifically,
several MPI-1 symbols were deprecated in the 1996 publishing of the
MPI-2.0 specification.  These deprecated symbols were eventually
removed from the MPI-3.0 specification in
2012.

The symbols that now no longer appear by default in Open MPI's
``mpi.h`` are:

* ``MPI_Address`` (replaced by ``MPI_Get_address``)
* ``MPI_Errhandler_create`` (replaced by ``MPI_Comm_create_errhandler``)
* ``MPI_Errhandler_get`` (replaced by ``MPI_Comm_get_errhandler``)
* ``MPI_Errhandler_set`` (replaced by ``MPI_Comm_set_errhandler``)
* ``MPI_Type_extent`` (replaced by ``MPI_Type_get_extent``)
* ``MPI_Type_hindexed`` (replaced by ``MPI_Type_create_hindexed``)
* ``MPI_Type_hvector`` (replaced by ``MPI_Type_create_hvector``)
* ``MPI_Type_lb`` (replaced by ``MPI_Type_get_extent``)
* ``MPI_Type_struct`` (replaced by ``MPI_Type_create_struct``)
* ``MPI_Type_ub`` (replaced by ``MPI_Type_get_extent``)
* ``MPI_LB`` (replaced by ``MPI_Type_create_resized``)
* ``MPI_UB`` (replaced by ``MPI_Type_create_resized``)
* ``MPI_COMBINER_HINDEXED_INTEGER``
* ``MPI_COMBINER_HVECTOR_INTEGER``
* ``MPI_COMBINER_STRUCT_INTEGER``
* ``MPI_Handler_function`` (replaced by ``MPI_Comm_errhandler_function``)

Although these symbols are no longer prototyped in ``mpi.h``, they
are still present in the MPI library in Open MPI |ompi_series|. This
enables legacy MPI applications to link and run successfully with
Open MPI |ompi_series|, even though they will fail to compile.

.. warning:: Future releases of Open MPI beyond the |ompi_series|
   series may remove these symbols altogether.

.. warning:: The Open MPI team **STRONGLY** encourages all MPI
   application developers to stop using these constructs that were
   first deprecated over 20 years ago, and finally removed from the
   MPI specification in MPI-3.0 (in 2012).

.. important:: :ref:`The "Removed MPI constructs" section
   <label-removed-mpi-constructs>` contains examples of how to update
   legacy MPI applications using these deleted symbols to use the
   "new" symbols.

All that being said, if you are unable to immediately update your
application to stop using these legacy MPI-1 symbols, you can
re-enable them in ``mpi.h`` by configuring Open MPI with the
``--enable-mpi1-compatibility`` flag.

Other MPI features
------------------

* Rank reordering support is available using the TreeMatch library. It
  is activated for the graph and ``dist_graph`` communicator topologies.

* When using MPI deprecated functions, some compilers will emit
  warnings.  For example:

  .. code-block::

     shell$ cat deprecated_example.c
     #include <mpi.h>
     void foo(void) {
         MPI_Datatype type;
         MPI_Type_struct(1, NULL, NULL, NULL, &type);
     }
     shell$ mpicc -c deprecated_example.c
     deprecated_example.c: In function 'foo':
     deprecated_example.c:4: warning: 'MPI_Type_struct' is deprecated (declared at /opt/openmpi/include/mpi.h:1522)
     shell$

* ``MPI_THREAD_MULTIPLE`` is supported with some exceptions.

  The following PMLs support ``MPI_THREAD_MULTIPLE``:

  #. ``cm``, when used with the following MTLs:

     #. ``ofi`` (Libfabric)
     #. ``portals4``

  #. ``ob1``, when used with the following BTLs:

     #. ``self``
     #. ``sm``
     #. ``smcuda``
     #. ``tcp``
     #. ``ugni``
     #. ``usnic``

  #. ``ucx``

  Currently, MPI File operations are not thread safe even if MPI is
  initialized for ``MPI_THREAD_MULTIPLE`` support.

* ``MPI_REAL16`` and ``MPI_COMPLEX32`` are only supported on platforms
  where a portable C datatype can be found that matches the Fortran
  type ``REAL*16``, both in size and bit representation.

* The "libompitrace" library is bundled in Open MPI and is installed
  by default (it can be disabled via the ``--disable-libompitrace``
  flag).  This library provides a simplistic tracing of select MPI
  function calls via the MPI profiling interface.  Linking it in to
  your application via (e.g., via ``-lompitrace``) will automatically
  output to stderr when some MPI functions are invoked:

  .. code-block::

     shell$ cd examples/
     shell$ mpicc hello_c.c -o hello_c -lompitrace
     shell$ mpirun -n 1 hello_c
     MPI_INIT: argc 1
     Hello, world, I am 0 of 1
     MPI_BARRIER[0]: comm MPI_COMM_WORLD
     MPI_FINALIZE[0]
     shell$

  Keep in mind that the output from the trace library is going to
  ``stderr``, so it may output in a slightly different order than the
  ``stdout`` from your application.

  This library is being offered as a "proof of concept" / convenience
  from Open MPI.  If there is interest, it is trivially easy to extend
  it to printf for other MPI functions.  Pull requests on github.com
  would be greatly appreciated.
