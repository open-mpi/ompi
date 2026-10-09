MPI-4.1 conformance gap analysis
================================

This document analyzes what is required to claim full conformance with
the `MPI-4.1 specification
<https://www.mpi-forum.org/docs/mpi-4.1/mpi41-report.pdf>`_ and lists the
APIs and functionality that are still missing from the Open MPI
|ompi_series| series.

.. note:: MPI-4.1 is a superset of MPI-4.0.  Full MPI-4.1 conformance
   therefore requires *everything* listed in the
   :doc:`MPI-4.0 gap analysis <mpi-4.0>` (most importantly the
   large-count ``_c`` interface and the ``MPI_T`` events interface) **in
   addition to** the MPI-4.1-specific items below.  This document covers
   only the items that MPI-4.1 adds on top of MPI-4.0.

Summary
-------

The Open MPI |ompi_series| series advertises MPI-3.1 conformance
(``MPI_VERSION`` ``3`` / ``MPI_SUBVERSION`` ``1``).  MPI-4.1 was largely
a consolidation release, but it introduced a focused set of new
procedures, constants, and deprecations.  **None** of the MPI-4.1
procedures listed below are implemented in Open MPI |ompi_series|, and
several of the MPI-4.1 deprecations are not yet reflected.

Missing procedures
------------------

Status field accessors (Section 3.2.5)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

MPI-4.1 added accessor procedures so that the ``MPI_Status`` fields can
be queried and set without direct structure access (which is important
for language interoperability and the large-count interface).  All six
are missing:

* ``MPI_Status_get_source`` / ``MPI_Status_set_source``
* ``MPI_Status_get_tag`` / ``MPI_Status_set_tag``
* ``MPI_Status_get_error`` / ``MPI_Status_set_error``

Communicator- and session-level message buffering (Section 3.6)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

MPI-4.1 added automatic (unlimited) buffering and the ability to attach
buffers at communicator and session scope rather than only globally.
The following procedures are missing:

* ``MPI_Comm_attach_buffer`` / ``MPI_Comm_detach_buffer``
* ``MPI_Session_attach_buffer`` / ``MPI_Session_detach_buffer``
* ``MPI_Comm_flush_buffer`` / ``MPI_Session_flush_buffer`` /
  ``MPI_Buffer_flush``
* The nonblocking variants ``MPI_Comm_iflush_buffer`` /
  ``MPI_Session_iflush_buffer`` / ``MPI_Buffer_iflush``

The associated predefined buffer constant ``MPI_BUFFER_AUTOMATIC`` is
also missing.  (The existing ``MPI_Buffer_attach`` / ``MPI_Buffer_detach``
remain, but their MPI-4.1 semantics — applying only to communicators
that have no communicator- or session-level buffer attached — are not
implemented.)

Querying multiple request statuses (Section 3.7.6)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

MPI-4.1 added non-destructive multi-request status queries (analogous to
``MPI_Testall`` / ``MPI_Testany`` / ``MPI_Testsome`` but without freeing
the requests).  All three are missing:

* ``MPI_Request_get_status_all``
* ``MPI_Request_get_status_any``
* ``MPI_Request_get_status_some``

(The single-request ``MPI_Request_get_status`` already exists.)

Value/index datatype query (Sections 5.1.13, 6.9.4)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* ``MPI_Type_get_value_index`` — query the predefined value-and-index
  pair datatype handles used with ``MPI_MINLOC`` / ``MPI_MAXLOC``.
* The associated combiner constant ``MPI_COMBINER_VALUE_INDEX`` is also
  missing.

Hardware resource information (Section 9.1.2)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* ``MPI_Get_hw_resource_info`` — return an info object describing the
  hardware resources available to the calling process.

Error class / code / string removal (Section 9.5)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

MPI-4.1 added the complements to the existing ``MPI_Add_error_*``
procedures.  All three are missing:

* ``MPI_Remove_error_class``
* ``MPI_Remove_error_code``
* ``MPI_Remove_error_string``

The associated error class ``MPI_ERR_ERRHANDLER`` is also missing.

Large-count status routine (Section 13.3)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

MPI-4.1 added the large-count interface for ``MPI_Status_set_elements``
that had been omitted from MPI-4.0:

* ``MPI_Status_set_elements_c``

This is part of the broader large-count gap described in the
:doc:`MPI-4.0 gap analysis <mpi-4.0>`.

Missing constants and info keys
-------------------------------

* ``MPI_COMM_TYPE_RESOURCE_GUIDED`` — a new ``split_type`` value for
  ``MPI_Comm_split_type``, together with its ``mpi_pset_name`` info key.
* ``MPI_BUFFER_AUTOMATIC`` (see above).
* ``MPI_COMBINER_VALUE_INDEX`` (see above).
* ``MPI_ERR_ERRHANDLER`` (see above).
* New info keys that are not recognized:
  ``mpi_memory_alloc_kinds`` and ``mpi_assert_memory_alloc_kinds``
  (memory-allocation-kinds support/assertion),
  ``mpi_accumulate_granularity`` (RMA accumulate granularity), and
  ``mpi_assert_strict_persistent_collective_ordering``.

Deprecations not yet reflected
------------------------------

MPI-4.1 deprecated a number of existing symbols.  These symbols still
exist in Open MPI |ompi_series| (which is correct — deprecated symbols
must remain usable), but they are not yet *marked* as deprecated, and
the user-visible deprecation guidance has not been updated:

* The ``_x`` element/extent routines, superseded by the large-count
  ``_c`` routines: ``MPI_Type_size_x``, ``MPI_Type_get_extent_x``,
  ``MPI_Type_get_true_extent_x``, ``MPI_Get_elements_x``, and
  ``MPI_Status_set_elements_x``.
* The ``MPI_HOST`` predefined attribute key.
* The ``mpif.h`` Fortran support method.

Other clarifications
--------------------

MPI-4.1 also contains many semantic clarifications that do not introduce
new symbols but may require implementation review for full conformance —
for example the relaxation of ``MPI_Win_shared_query`` to windows of
flavor ``MPI_WIN_FLAVOR_CREATE`` and ``MPI_WIN_FLAVOR_ALLOCATE``,
allowing ``MPI_COMM_NULL`` / ``MPI_DATATYPE_NULL`` / ``MPI_WIN_NULL`` to
be passed to the corresponding ``MPI_*_get_name`` routines, and the
clarified strong/weak progress rules for ``MPI_Request_get_status`` and
``MPI_Win_test``.  These are behavioral and are not enumerated as
missing APIs here.
