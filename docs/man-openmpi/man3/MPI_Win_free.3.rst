.. _mpi_win_free:


MPI_Win_free
============

.. include_body

:ref:`MPI_Win_free` |mdash| Frees the window object and returns a null handle.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_free.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_free` frees the window object *win* and returns a null handle
(equal to MPI_WIN_NULL). This collective call is executed by all
processes in the group associated with *win*. It can be invoked by a
process only after it has completed its involvement in RMA
communications on window *win*, that is, the process has called
:ref:`MPI_Win_fence`, or called :ref:`MPI_Win_unlock` to match a previous call to
:ref:`MPI_Win_lock`. When the call returns, the window memory can be freed.


NOTES
-----

If the window was created through :ref:`MPI_Win_allocate` or
:ref:`MPI_Win_allocate_shared` then the memory buffer allocated in that
call will be freed when calling :ref:`MPI_Win_free`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_create`
   * :ref:`MPI_Win_allocate`
   * :ref:`MPI_Win_allocate_shared`
