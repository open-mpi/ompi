.. _mpi_comm_dup:

MPI_Comm_dup
============

.. include_body

:ref:`MPI_Comm_dup` - Duplicates an existing communicator with all its cached
information.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_DUP(COMM, NEWCOMM, IERROR)
       INTEGER COMM, NEWCOMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Comm_dup(comm, newcomm, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  comm : Communicator (handle).

Output Parameters
-----------------

-  newcomm : Copy of comm (handle).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Comm_dup` duplicates the existing communicator comm with associated
key values. For each key value, the respective copy callback function
determines the attribute value associated with this key in the new
communicator; one particular action that a copy callback may take is to
delete the attribute from the new communicator. Returns in newcomm a new
communicator with the same group, any copied cached information, but a
new context (see Section 5.7.1 of the MPI-1 Standard, "Functionality").

Notes
-----

This operation is used to provide a parallel library call with a
duplicate communication space that has the same properties as the
original communicator. This includes any attributes (see below) and
topologies (see Chapter 6, "Process Topologies," in the MPI-1 Standard).
This call is valid even if there are pending point-to-point
communications involving the communicator comm. A typical call might
involve an :ref:`MPI_Comm_dup` at the beginning of the parallel call, and an
:ref:`MPI_Comm_free` of that duplicated communicator at the end of the call.
Other models of communicator management are also possible. This call
applies to both intra- and intercommunicators. Note that it is not
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


.. seealso:: :ref:`MPI_Comm_dup_with_info`
