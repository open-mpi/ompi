.. _mpi_comm_dup_with_info:

MPI_Comm_dup_with_info
======================

.. include_body

:ref:`MPI_Comm_dup_with_info` - Duplicates an existing communicator using
provided info.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Comm_dup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_DUP_WITH_INFO(COMM, INFO, NEWCOMM, IERROR)
       INTEGER COMM, INFO, NEWCOMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Comm_dup_with_info(comm, info, newcomm, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Info), INTENT(IN) :: info
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  comm : Communicator (handle).
-  info : Info argument (handle).

Output Parameters
-----------------

-  newcomm : Copy of comm (handle).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Comm_dup_with_info` acts exactly like :ref:`MPI_Comm_dup` except that the
info hints associated with the communicator comm are not duplicated in
newcomm. The hints provided by the argument info are associated with the
output communicator newcomm instead. See :ref:`MPI_Comm_set_info` for the
list of recognized info keys.

Notes
-----

This operation is used to provide a parallel library call with a
duplicate communication space that has the same properties as the
original communicator. This includes any attributes (see below) and
topologies (see Chapter 6, "Process Topologies," in the MPI-1 Standard).
This call is valid even if there are pending point-to-point
communications involving the communicator comm. A typical call might
involve an :ref:`MPI_Comm_dup_with_info` at the beginning of the parallel call,
and an :ref:`MPI_Comm_free` of that duplicated communicator at the end of the
call. Other models of communicator management are also possible. This
call applies to both intra- and intercommunicators. Note that it is not
defined by the MPI standard what happens if the attribute copy callback
invokes other MPI functions. In Open MPI, it is not valid for attribute
copy callbacks (or any of their children) to add or delete attributes on
the same object on which the attribute copy callback is being invoked.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with :ref:`MPI_Comm_set_errhandler`;
the predefined error handler MPI_ERRORS_RETURN may be used to cause
error values to be returned. Note that MPI does not guarantee that an
MPI program can continue past an error.


.. seealso:: :ref:`MPI_Comm_dup`
