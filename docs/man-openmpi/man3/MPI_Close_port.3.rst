.. _mpi_close_port:

MPI_Close_port
==============

.. include_body

:ref:`MPI_Close_port` - Releases the specified network address.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Close_port(const char *port_name)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CLOSE_PORT(PORT_NAME, IERROR)
       CHARACTER*(*)   PORT_NAME
       INTEGER     IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Close_port(port_name, ierror)
       CHARACTER(LEN=*), INTENT(IN) :: port_name
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  ``port_name`` : A port (string).

Output Parameter
----------------

-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Close_port` releases the network address represented by
``port_name``.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
