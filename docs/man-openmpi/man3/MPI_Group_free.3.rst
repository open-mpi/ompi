.. _mpi_group_free:

MPI_Group_free
==============

.. include_body

:ref:`MPI_Group_free` - Frees a group.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Group_free(MPI_Group *group)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GROUP_FREE(GROUP, IERROR)
       INTEGER GROUP, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Group_free(group, ierror)
       TYPE(MPI_Group), INTENT(INOUT) :: group
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input/Output Parameter
----------------------

-  ``group`` : Group (handle).

Output Parameter
----------------

-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

This operation marks a ``group`` object for deallocation. The handle
``group`` is set to MPI_GROUP_NULL by the call. Any ongoing
operation using this ``group`` will complete normally.

Note
----

On return, ``group`` is set to MPI_GROUP_NULL.

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
