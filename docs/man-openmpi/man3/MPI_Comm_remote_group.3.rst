.. _mpi_comm_remote_group:


MPI_Comm_remote_group
=====================

.. include_body

:ref:`MPI_Comm_remote_group` - Accesses the remote group associated with an
intercommunicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_REMOTE_GROUP(COMM, GROUP, IERROR)
   	INTEGER	COMM, GROUP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_remote_group(comm, group, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Group), INTENT(OUT) :: group
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator.

OUTPUT PARAMETERS
-----------------
* ``group``: Remote group of communicator.
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_remote_group` accesses the remote group associated with an
intercommunicator.

The intercommunicator accessors (:ref:`MPI_Comm_test_inter`,
:ref:`MPI_Comm_remote_size`, MPI_Comm_remote_group) are all local operations.


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


.. seealso::
   :ref:`MPI_Comm_test_inter` :ref:`MPI_Comm_remote_size` :ref:`MPI_Intercomm_create`
   :ref:`MPI_Intercomm_merge`
