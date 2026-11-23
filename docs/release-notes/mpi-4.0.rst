MPI-4.0 conformance gap analysis
================================

This document analyzes what is required to claim full conformance with
the `MPI-4.0 specification
<https://www.mpi-forum.org/docs/mpi-4.0/mpi40-report.pdf>`_ and lists the
APIs and functionality that are still missing from the Open MPI
|ompi_series| series.

.. note:: This is a *gap* analysis: it enumerates what is **not yet**
   implemented.  For the list of MPI-4.0 features that *are* already
   supported, see :ref:`the MPI Standard conformance section
   <release-notes-mpi-standard-conformance-label>`.

Summary
-------

The Open MPI |ompi_series| series formally advertises MPI-3.1
conformance: ``mpi.h`` defines ``MPI_VERSION`` as ``3`` and
``MPI_SUBVERSION`` as ``1``.  A large portion of MPI-4.0 is nonetheless
implemented (Sessions, partitioned communication, persistent
collectives, large-count *constants*, the new info routines, and the new
error handlers).

Two major feature families and a small number of supporting symbols are
missing.  Until they are implemented, Open MPI cannot advertise
``MPI_VERSION`` ``4`` / ``MPI_SUBVERSION`` ``0``:

#. The **large-count ("embiggening") interface** — the entire family of
   ``MPI_<fn>_c`` bindings.
#. The **MPI tool information interface "events" interface** — the
   callback-driven ``MPI_T_event_*`` / ``MPI_T_source_*`` interface.

The two sections below describe these gaps in detail.  A final section
lists the smaller supporting items.

Missing: the large-count ("embiggening") interface
--------------------------------------------------

MPI-4.0 (Section 2.5.8) added large-count variants of every procedure
that takes a count, size, displacement, or extent argument.  In C these
are spelled with a ``_c`` suffix and use ``MPI_Count`` /
``MPI_Aint`` arguments in place of ``int``; in the Fortran ``mpi_f08``
module they are provided through function overloading (so no new Fortran
names appear, with two explicit exceptions noted below).

Open MPI |ompi_series| implements **none** of the ``_c`` bindings: there
are zero ``_c`` prototypes in the installed ``mpi.h``.  This is the
single largest MPI-4.0 conformance gap, affecting more than 150
procedures, including (non-exhaustively):

* Point-to-point: ``MPI_Send_c``, ``MPI_Recv_c``, ``MPI_Isend_c``,
  ``MPI_Irecv_c``, ``MPI_Sendrecv_c``, ``MPI_Bsend_c``, ``MPI_Ssend_c``,
  ``MPI_Rsend_c``, ``MPI_Send_init_c``, ``MPI_Recv_init_c``,
  ``MPI_Isendrecv_c``, ``MPI_Get_count_c``, ``MPI_Get_elements_c``,
  ``MPI_Pack_c``, ``MPI_Unpack_c``, ``MPI_Pack_size_c``, ``MPI_Mrecv_c``,
  ``MPI_Imrecv_c``, and the partitioned ``MPI_Psend_init_c`` /
  ``MPI_Precv_init_c``.
* Collectives and their nonblocking / persistent forms:
  ``MPI_Bcast_c``, ``MPI_Reduce_c``, ``MPI_Allreduce_c``,
  ``MPI_Gather_c``, ``MPI_Gatherv_c``, ``MPI_Scatter_c``,
  ``MPI_Scatterv_c``, ``MPI_Allgather_c``, ``MPI_Allgatherv_c``,
  ``MPI_Alltoall_c``, ``MPI_Alltoallv_c``, ``MPI_Alltoallw_c``,
  ``MPI_Scan_c``, ``MPI_Exscan_c``, ``MPI_Reduce_scatter_c``,
  ``MPI_Reduce_scatter_block_c``, ``MPI_Reduce_local_c``, the
  ``MPI_I*_c`` nonblocking variants, the ``MPI_*_init_c`` persistent
  variants, and the ``MPI_Neighbor_*_c`` / ``MPI_Ineighbor_*_c`` /
  ``MPI_Neighbor_*_init_c`` neighborhood variants.
* Datatypes: ``MPI_Type_contiguous_c``, ``MPI_Type_vector_c``,
  ``MPI_Type_create_hvector_c``, ``MPI_Type_indexed_c``,
  ``MPI_Type_create_hindexed_c``, ``MPI_Type_create_indexed_block_c``,
  ``MPI_Type_create_hindexed_block_c``, ``MPI_Type_create_struct_c``,
  ``MPI_Type_create_subarray_c``, ``MPI_Type_create_darray_c``,
  ``MPI_Type_create_resized_c``, ``MPI_Type_get_extent_c``,
  ``MPI_Type_get_true_extent_c``, ``MPI_Type_size_c``,
  ``MPI_Type_get_contents_c``, ``MPI_Type_get_envelope_c``.
* One-sided (RMA): ``MPI_Put_c``, ``MPI_Get_c``, ``MPI_Accumulate_c``,
  ``MPI_Get_accumulate_c``, ``MPI_Rput_c``, ``MPI_Rget_c``,
  ``MPI_Raccumulate_c``, ``MPI_Rget_accumulate_c``,
  ``MPI_Win_create_c``, ``MPI_Win_allocate_c``,
  ``MPI_Win_allocate_shared_c``, ``MPI_Win_shared_query_c``.
* I/O: the ``_c`` forms of all ``MPI_File_read*`` /
  ``MPI_File_write*`` procedures, ``MPI_File_get_type_extent_c``.
* Supporting routines: ``MPI_Op_create_c`` and
  ``MPI_Register_datarep_c`` (these two *do* introduce new explicit
  Fortran names, unlike the overloaded majority).

The large-count interface also requires the following supporting
symbols, which are likewise missing:

* The callback prototypes ``MPI_User_function_c`` and
  ``MPI_Datarep_conversion_function_c``.
* The predefined conversion function ``MPI_CONVERSION_FN_NULL_C``.

The supporting error class ``MPI_ERR_VALUE_TOO_LARGE`` **is** already
defined in ``mpi.h``.

Missing: the MPI tool information interface "events" interface
--------------------------------------------------------------

MPI-4.0 (Sections 15.3.8–15.3.9) added a callback-driven event interface
to the MPI tool information interface ("MPI_T").  Open MPI
|ompi_series| implements the control-variable, performance-variable, and
category portions of MPI_T, but the entire events interface is absent
(there are no event sources in ``ompi/mpi/tool/`` and no
``MPI_T_event*`` symbols in ``mpi.h``).

The following procedures are missing:

* Event sources: ``MPI_T_source_get_num``, ``MPI_T_source_get_info``,
  ``MPI_T_source_get_timestamp``.
* Event types: ``MPI_T_event_get_num``, ``MPI_T_event_get_info``,
  ``MPI_T_event_get_index``.
* Event handles and registration: ``MPI_T_event_handle_alloc``,
  ``MPI_T_event_handle_set_info``, ``MPI_T_event_handle_get_info``,
  ``MPI_T_event_handle_free``, ``MPI_T_event_register_callback``,
  ``MPI_T_event_callback_set_info``, ``MPI_T_event_callback_get_info``,
  ``MPI_T_event_set_dropped_handler``.
* Reading event data in a callback: ``MPI_T_event_read``,
  ``MPI_T_event_copy``, ``MPI_T_event_get_timestamp``,
  ``MPI_T_event_get_source``.
* Category integration: ``MPI_T_category_get_num_events`` and
  ``MPI_T_category_get_events``.

The supporting types and callback prototypes are also missing:
``MPI_T_cb_safety``, ``MPI_T_event_instance``,
``MPI_T_event_registration``, ``MPI_T_source_order``, and the callback
function prototypes ``MPI_T_event_cb_function``,
``MPI_T_event_free_cb_function``, and
``MPI_T_event_dropped_cb_function``.

Other smaller gaps
------------------

* ``MPI_T_BIND_MPI_SESSION`` — this binding constant (introduced
  alongside the Sessions model so that tool variables can be bound to a
  session) is not defined in ``mpi.h``.

What is already supported
-------------------------

For completeness, the following MPI-4.0 additions are **already**
implemented in the Open MPI |ompi_series| series and are therefore *not*
gaps:

* The Sessions model: ``MPI_Session_init`` / ``MPI_Session_finalize``,
  the ``MPI_Session_get_*`` query routines, the session error-handler
  routines, ``MPI_Group_from_session_pset``,
  ``MPI_Comm_create_from_group``,
  ``MPI_Intercomm_create_from_groups``, and ``MPI_Session_c2f`` /
  ``MPI_Session_f2c``.
* Partitioned communication: ``MPI_Psend_init``, ``MPI_Precv_init``,
  ``MPI_Pready``, ``MPI_Pready_range``, ``MPI_Pready_list``, and
  ``MPI_Parrived``.
* Persistent collectives (and persistent neighborhood collectives) in
  the ``MPI_`` namespace, e.g. ``MPI_Bcast_init``, ``MPI_Reduce_init``,
  ``MPI_Allreduce_init``, ``MPI_Neighbor_allgather_init``.
* ``MPI_Isendrecv`` and ``MPI_Isendrecv_replace``.
* ``MPI_Comm_idup_with_info``.
* ``MPI_Info_get_string`` and ``MPI_Info_create_env``.
* The ``MPI_ERRORS_ABORT`` error handler, ``MPI_COMM_SELF`` as the
  default error-handling object for object-less calls, and the
  ``mpi_initial_errhandler`` info key.
* The ``MPI_COMM_TYPE_HW_GUIDED`` and ``MPI_COMM_TYPE_HW_UNGUIDED``
  split types and the ``mpi_hw_resource_type`` info key.
* The ``mpi_minimum_memory_alignment`` info key.
* The ``MPI_ERR_VALUE_TOO_LARGE`` and ``MPI_ERR_PROC_ABORTED`` error
  classes.
* Deprecation of ``MPI_Sizeof``, of ``MPI_Cancel`` on send requests, and
  of ``MPI_Info_get`` / ``MPI_Info_get_valuelen``.
