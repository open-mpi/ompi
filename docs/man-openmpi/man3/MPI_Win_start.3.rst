.. _mpi_win_start:


MPI_Win_start
=============

.. include_body

:ref:`MPI_Win_start` |mdash| Starts an RMA access epoch for *win*


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_start(MPI_Group group, int assert, MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_START(GROUP, ASSERT, WIN, IERROR)
   	INTEGER GROUP, ASSERT, WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_start(group, assert, win, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group
   	INTEGER, INTENT(IN) :: assert
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``group``: The group of target processes (handle).
* ``assert``: Program assertion (integer).
* ``win``: Window object (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_start` is a one-sided MPI communication synchronization call that
starts an RMA access epoch for *win*. RMA calls issued on *win* during
this epoch must access only windows at processes in *group*. Each
process in *group* must issue a matching call to :ref:`MPI_Win_post`.
:ref:`MPI_Win_start` is allowed to block until the corresponding :ref:`MPI_Win_post`
calls have been executed, but is not required to.

The *assert* argument is used to provide assertions on the context of
the call that may be used for various optimizations. (See Section 6.4.4
of the MPI-2 Standard.) A value of *assert* = 0 is always valid. The
following assertion value is supported:

MPI_MODE_NOCHECK
   When this value is passed in to this call, the library assumes that
   the post call on the target has been called and it is not necessary
   for the library to check to see if such a call has been made.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_post`
   * :ref:`MPI_Win_complete`
