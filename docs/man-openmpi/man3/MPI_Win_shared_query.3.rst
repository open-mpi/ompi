.. _mpi_win_shared_query:


MPI_Win_shared_query
====================

.. include_body

:ref:`MPI_Win_shared_query` |mdash| Query a shared memory window


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_shared_query (MPI_Win win, int rank, MPI_Aint *size,
                             int *disp_unit, void *baseptr)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_SHARED_QUERY(WIN, RANK, SIZE, DISP_UNIT, BASEPTR, IERROR)
           INTEGER WIN, RANK, DISP_UNIT, IERROR
           INTEGER(KIND=MPI_ADDRESS_KIND) SIZE, BASEPTR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_shared_query(win, rank, size, disp_unit, baseptr, ierror)
   	USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, INTENT(IN) :: rank
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: size
   	INTEGER, INTENT(OUT) :: disp_unit
   	TYPE(C_PTR), INTENT(OUT) :: baseptr
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


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
:ref:`MPI_Win_allocate_shared`. This function can return different
process-local addresses for the same physical memory on different
processes. The returned memory can be used for load/store accesses
subject to the constraints defined in MPI-3.1 section 11.7. This
function can only be called with windows of flavor
MPI_WIN_FLAVOR_SHARED. If the passed window is not of flavor
MPI_WIN_FLAVOR_SHARED, the error MPI_ERR_RMA_FLAVOR is raised. When
rank is ``MPI_PROC_NULL``, the *pointer*, *disp_unit*, and *size* returned
are the pointer, disp_unit, and size of the memory segment belonging
the lowest rank that specified *size* > 0. If all processes in the
group attached to the window specified *size* = 0, then the call
returns *size* = 0 and a *baseptr* as if :ref:`MPI_Alloc_mem` was
called with *size* = 0.


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
