.. _mpi_win_flush_local:


MPI_Win_flush_local
===================

.. include_body

:ref:`MPI_Win_flush_local`, :ref:`MPI_Win_flush_local_all` - Complete all
outstanding RMA operations at both the origin


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_flush_local (int rank, MPI_Win win)

   int MPI_Win_flush_local_all (MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_FLUSH_LOCAL(RANK, WIN, IERROR)
   	INTEGER RANK, WIN, IERROR

   MPI_WIN_FLUSH_LOCAL_ALL(WIN, IERROR)
   	INTEGER WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_flush_local(rank, win, ierror)
   	INTEGER, INTENT(IN) :: rank
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Win_flush_local_all(win, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``rank``: Rank of window (nonnegative integer).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_flush_local` locally completes at the origin all outstanding
RMA operations initiated by the calling process to the target process
specified by rank on the specified window. For example, after this
routine completes, the user may reuse any buffers provided to put, get,
or accumulate operations. :ref:`MPI_Win_flush_local_all` locally completes
at the origin all outstanding RMA operations to all targets.

Can only be called from within a passive target epoch.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_flush`
   * :ref:`MPI_Win_lock`
   * :ref:`MPI_Win_lock_all`
