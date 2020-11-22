.. _mpi_win_test:


MPI_Win_test
============

.. include_body

:ref:`MPI_Win_test` - Attempts to complete an RMA exposure epoch; a
nonblocking version of :ref:`MPI_Win_wait`


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_test(MPI_Win win, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_TEST( WIN, FLAG, IERROR)
   	INTEGER  WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_test(win, flag, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: Window object (handle)

OUTPUT PARAMETERS
-----------------
* ``IERROR``: Fortran only: Error status (integer).
* ``flag``: The returning state of the test for epoch closure.

DESCRIPTION
-----------

:ref:`MPI_Win_test` is a one-sided MPI communication synchronization call, a
nonblocking version of :ref:`MPI_Win_wait`. It returns *flag = true* if
:ref:`MPI_Win_wait` would return, *flag = false* otherwise. The effect of
return of :ref:`MPI_Win_test` with *flag = true* is the same as the effect of a
return of :ref:`MPI_Win_wait`. If *flag = false* is returned, then the call has
no visible effect.

Invoke :ref:`MPI_Win_test` only where :ref:`MPI_Win_wait` can be invoked. Once the
call has returned *flag = true*, it must not be invoked anew, until the
window is posted anew.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Win_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Win_post` :ref:`MPI_Win_wait`
