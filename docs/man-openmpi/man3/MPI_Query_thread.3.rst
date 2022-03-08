.. _mpi_query_thread:


MPI_Query_thread
================

.. include_body

:ref:`MPI_Query_thread` - Returns the current level of thread support


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Query_thread(int *provided)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_QUERY_THREAD(PROVIDED, IERROR)
   	INTEGER	PROVIDED, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Query_thread(provided, ierror)
   	INTEGER, INTENT(OUT) :: provided
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


OUTPUT PARAMETERS
-----------------
* ``provided``: C/Fortran only: Level of thread support (integer).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine returns in *provided* the current level of thread support.
If MPI was initialized by a call to :ref:`MPI_Init_thread`, *provided* will
have the same value as was returned by that function.

The possible values of *provided* are as follows:

MPI_THREAD_SINGLE
   Only one thread may execute.

MPI_THREAD_FUNNELED
   If the process is multithreaded, only the thread that called
   MPI_Init[_thread] may make MPI calls.

MPI_THREAD_SERIALIZED
   If the process is multithreaded, only one thread may make MPI library
   calls at one time.

MPI_THREAD_MULTIPLE
   If the process is multithreaded, multiple threads may call MPI at
   once with no restrictions.


NOTES
-----

In Open MPI, *provided* is always MPI_THREAD_SINGLE, unless the program
has been linked with the multithreaded library, in which case *provided*
is MPI_THREAD_MULTIPLE.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

See the MPI man page for a full list of MPI error codes.


.. seealso::
   :ref:`MPI_Init` :ref:`MPI_Init_thread`
