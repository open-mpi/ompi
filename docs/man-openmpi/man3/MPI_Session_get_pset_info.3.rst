.. _mpi_session_get_pset_info:

MPI_Session_get_pset_info
=========================

.. include_body

:ref:`MPI_Session_get_pset_info` - Returns an info object containing properties
of a specific process set

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Session_get_pset_info(MPI_Session session, const char *pset_name, MPI_Info *info)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_GET_PSET_INFO(SESSION, PSET_NAME, INFO, IERROR)
       INTEGER SESSION, INFO, IERROR
       CHARACTER*(*) PSET_NAME

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Session_get_pset_info(session, pset_name, info, ierror)
       TYPE(MPI_Session), INTENT(IN) :: session
       CHARACTER(LEN=*), INTENT(IN) :: pset_name
       TYPE(MPI_Info), INTENT(OUT) :: info
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  session : session (handle)
-  pset_name : name of process set (string)

Output Parameters
-----------------

-  info: info object (handle)
-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Session_get_pset_info` is used to query properties of a specific
process set. The returned info object can be queried with existing MPI
info object query functions. One key/value pair must be de ned,
"mpi_size". The value of the "mpi_size" key specifies the number of MPI
processes in the process set.

Notes
-----

The user is responsible for freeing the returned info object via
:ref:`MPI_Info_free`.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with
MPI_Session_set_errhandler; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned. Note
that MPI does not guarantee that an MPI program can continue past an
error.


.. seealso:: :ref:`MPI_Session_init`
