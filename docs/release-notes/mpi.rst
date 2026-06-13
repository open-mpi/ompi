MPI Functionality and Features
==============================

.. _release-notes-mpi-standard-conformance-label:

MPI Standard conformance
------------------------

The Open MPI |ompi_series| series targets the
MPI-|mpi_standard_version| standard.  Accordingly, ``MPI_VERSION`` is
set to |mpi_standard_major_version| and ``MPI_SUBVERSION`` is set to
|mpi_standard_minor_version|.  These values and the ``MPI_VERSION`` /
``MPI_SUBVERSION`` macros that ``configure`` writes into the installed
``mpi.h`` are both derived from the top-level ``VERSION`` file, so the
documentation and the header cannot disagree.

The differences that remain from a *full* conformance claim are matters
of runtime functionality and build configuration, not missing APIs.
The most notable are:

* The MPI tool *events* interface (``MPI_T_event_*``) is provided, but a
  given build may expose no event sources (which the MPI standard
  permits).
* The standardized MPI ABI (new in MPI-5.0) is not yet available on the
  ``main`` branch; it is in flight in an as-yet-unmerged pull request.
* Data representation callbacks, RMA, MPI-IO, and dynamic process
  management depend on configured components and can be disabled at
  configure time.
* Some optional Fortran datatypes (for example, ``MPI_REAL16``) depend
  on the Fortran compiler and build.

The :ref:`MPI-5.0 conformance analysis
<release-notes-mpi-5.0-analysis-label>` linked above documents these
caveats in detail.

For historical reference, earlier MPI standard conformance levels were
introduced in the following Open MPI versions:

.. list-table::
   :header-rows: 1

   * - MPI standards conformance
     - Introduced in Open MPI version

   * - MPI-3.1
     - Open MPI v2.0

   * - MPI-3.0
     - Open MPI v1.8

   * - MPI-2.1
     - Open MPI v1.3

   * - MPI-2.0
     - Open MPI v1.2

Removed MPI APIs
----------------

Note that starting with Open MPI v4.0.0, prototypes for several
legacy MPI-1 symbols that were deleted in the MPI-3.0 specification
are no longer available by default in ``mpi.h``.  Specifically,
several MPI-1 symbols were deprecated in the 1996 publishing of the
MPI-2.0 specification.  These deprecated symbols were eventually
removed from the MPI-3.0 specification in 2012.

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
   first deprecated over 30 years ago, and finally removed from the
   MPI specification in MPI-3.0 (in 2012).

.. important:: :ref:`The "Removed MPI constructs" section
   <label-removed-mpi-constructs>` contains examples of how to update
   legacy MPI applications using these deleted symbols to use the
   "new" symbols.

All that being said, if you are unable to immediately update your
application to stop using these legacy MPI-1 symbols, you can
re-enable them in ``mpi.h`` by configuring Open MPI with the
``--enable-mpi1-compatibility`` flag.
