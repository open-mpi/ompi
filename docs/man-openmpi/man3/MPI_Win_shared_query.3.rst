.. _mpi_win_shared_query:


MPI_Win_shared_query
====================

.. include_body

:ref:`MPI_Win_shared_query` |mdash| Query a shared memory window

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_shared_query.rst

INPUT PARAMETERS
----------------
* ``win``: Shared memory window object (handle).
* ``rank``: Rank in the group of window *win* (non-negative integer) or ``MPI_PROC_NULL``.

OUTPUT PARAMETERS
-----------------
* ``size``: Size of the window segment (non-negative integer).
* ``disp_unit``: Local unit size for displacements, in bytes (positive integer).
* ``baseptr``: Address for load/store access to window segment (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_shared_query` queries the process-local address for
remote memory segments created with
:ref:`MPI_Win_create`, :ref:`MPI_Win_allocate`, or
:ref:`MPI_Win_allocate_shared`. Shared memory is only guaranteed to be
available for windows created with :ref:`MPI_Win_allocate_shared`.
This function can return different
process-local addresses for the same physical memory on different
processes. The returned memory can be used for load/store accesses
subject to the constraints defined in MPI-3.1 section 11.7.
If the passed a dynamic window, the error MPI_ERR_RMA_FLAVOR is raised. When
rank is ``MPI_PROC_NULL``, the *pointer*, *disp_unit*, and *size* returned
are the pointer, disp_unit, and size of the memory segment belonging
to the lowest rank that specified *size* > 0. If all processes in the
group attached to the window specified *size* = 0, then the call
returns *size* = 0 and a *baseptr* as if :ref:`MPI_Alloc_mem` was
called with *size* = 0.
If the window was created with :ref:`MPI_Win_create` or
:ref:`MPI_Win_allocate` and shared memory is not supported for these windows,
the call returns *size* = 0 and a *baseptr* as if
:ref:`MPI_Alloc_mem` was called with *size* = 0.


C NOTES
-------

The parameter *baseptr* is of type ``void *`` to allow passing any
pointer object for this parameter. The provided argument should be a
pointer to a pointer of arbitrary type (e.g. ``void **``).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Alloc_mem`
   * :ref:`MPI_Win_allocate_shared`
   * :ref:`MPI_Win_create`
   * :ref:`MPI_Win_allocate`
