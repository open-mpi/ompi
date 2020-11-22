.. _mpi_get_processor_name:

MPI_Get_processor_name
======================

.. include_body

:ref:`MPI_Get_processor_name` - Gets the name of the processor.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Get_processor_name(char *name, int *resultlen)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_PROCESSOR_NAME(NAME, RESULTLEN, IERROR)
       CHARACTER*(*)   NAME
       INTEGER     RESULTLEN, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Get_processor_name(name, resultlen, ierror)
       CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME), INTENT(OUT) :: name
       INTEGER, INTENT(OUT) :: resultlen
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Output Parameters
-----------------

-  ``name`` : A unique specifier for the actual (as opposed to virtual)
   node.
-  ``resultlen`` : Length (in characters) of result returned in name.
-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

This routine returns the ``name`` of the processor on which it was
called at the moment of the call. The ``name`` is a character string for
maximum flexibility. From this value it must be possible to identify a
specific piece of hardware. The argument ``name`` must represent storage
that is at least MPI_MAX_PROCESSOR_NAME characters long.

The number of characters actually written is returned in the output
argument, ``resultlen``.

Notes
-----

The user must provide at least MPI_MAX_PROCESSOR_NAME space to write
the processor ``name``; processor ``name``\ s can be this long. The user
should examine the output argument, ``resultlen``, to determine the
actual length of the ``name``.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
