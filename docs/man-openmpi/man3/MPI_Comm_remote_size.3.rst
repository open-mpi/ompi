.. _mpi_comm_remote_size:


MPI_Comm_remote_size
====================

.. include_body

:ref:`MPI_Comm_remote_size` |mdash| Determines the size of the remote group
associated with an intercommunicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_remote_size(MPI_Comm comm, int *size)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_REMOTE_SIZE(COMM, SIZE, IERROR)
   	INTEGER	COMM, SIZE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_remote_size(comm, size, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Number of processes in the remote group of comm (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_remote_size` determines the size of the remote group associated
with an intercommunicator.

The intercommunicator accessors (:ref:`MPI_Comm_test_inter`,
:ref:`MPI_Comm_remote_size`, MPI_Comm_remote_group) are all local operations.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_test_inter`
   * :ref:`MPI_Comm_remote_group`
   * :ref:`MPI_Intercomm_create`
   * :ref:`MPI_Intercomm_merge`
