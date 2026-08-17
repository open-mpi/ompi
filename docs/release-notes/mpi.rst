MPI Functionality and Features
==============================

.. _release-notes-mpi-standard-conformance-label:

MPI Standard conformance
------------------------

In the Open MPI |ompi_series| series, all MPI-|mpi_standard_version|
functionality is fully supported and conformant.  As such,
``MPI_VERSION`` is set to 3 and ``MPI_SUBVERSION`` is set to 1.

Substantial portions of MPI-4.0 and MPI-4.1 are also implemented.
However, neither standard is fully conformant, so Open MPI does not yet
advertise an ``MPI_VERSION`` of 4.  The gaps are summarized below and
enumerated in detail in dedicated documents.

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

MPI-4.0 and MPI-4.1 partial conformance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Open MPI |ompi_series| series implements a large fraction of
MPI-4.0, including:

* The Sessions model.
* Partitioned communication (persistent sends and receives).
* Persistent collective and neighborhood collective operations in the
  ``MPI_`` namespace (these were previously available only via the
  ``MPIX_`` prefix).
* :ref:`MPI_Isendrecv()<mpi_isendrecv>` and its variants.
* :ref:`MPI_Comm_idup_with_info()<mpi_comm_idup_with_info>`,
  :ref:`MPI_Info_get_string()<mpi_info_get_string>`, and
  :ref:`MPI_Info_create_env()<mpi_info_create_env>`.
* The ``MPI_ERRORS_ABORT`` error handler, ``MPI_COMM_SELF``-based
  handling of "unbound" errors, and the ``mpi_initial_errhandler``
  info key.
* The ``MPI_COMM_TYPE_HW_GUIDED`` and ``MPI_COMM_TYPE_HW_UNGUIDED``
  communicator split types and the ``mpi_minimum_memory_alignment``
  info key.
* The ``MPI_F_STATUS_SIZE``, ``MPI_F_SOURCE``, ``MPI_F_TAG``, and
  ``MPI_F_ERROR`` constants.
* The MPI-4.0 deprecations of :ref:`MPI_Sizeof()<mpi_sizeof>`,
  :ref:`MPI_Cancel()<mpi_cancel>` on send requests, and
  :ref:`MPI_Info_get()<mpi_info_get>` /
  :ref:`MPI_Info_get_valuelen()<mpi_info_get_valuelen>`.

Two major MPI-4.0 feature families are *not* yet implemented, and their
absence is the primary reason Open MPI cannot advertise MPI-4.0
conformance:

* The **large-count ("embiggening") interface**: the entire family of
  ``MPI_<fn>_c`` C bindings (and their Fortran ``mpi_f08`` overloads)
  that accept ``MPI_Count`` arguments in place of ``int``.
* The **MPI tool information interface "events" interface**: the
  callback-driven ``MPI_T_event_*`` and ``MPI_T_source_*`` routines.

MPI-4.1 is a superset of MPI-4.0.  Full MPI-4.1 conformance therefore
requires closing the MPI-4.0 gaps above *and* implementing the
procedures that MPI-4.1 newly introduced -- none of which are yet
available.  These include the
``MPI_Status_{get,set}_{source,tag,error}`` accessors, communicator-
and session-level message buffering, the
``MPI_Request_get_status_{all,any,some}`` queries,
``MPI_Type_get_value_index``, ``MPI_Get_hw_resource_info``, and the
``MPI_Remove_error_{class,code,string}`` routines.

For the complete, itemized gap analyses -- including every missing
procedure, constant, info key, and deprecation -- see:

* :doc:`mpi-4.0`
* :doc:`mpi-4.1`

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
