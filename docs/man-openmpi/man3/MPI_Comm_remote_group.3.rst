.. _mpi_comm_remote_group:


MPI_Comm_remote_group
=====================

.. include_body

:ref:`MPI_Comm_remote_group` |mdash| Accesses the remote group associated with an
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
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_remote_group` accesses the remote group associated with an
intercommunicator.

The intercommunicator accessors (:ref:`MPI_Comm_test_inter`,
:ref:`MPI_Comm_remote_size`, MPI_Comm_remote_group) are all local operations.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_test_inter`
   * :ref:`MPI_Comm_remote_size`
   * :ref:`MPI_Intercomm_create`
   * :ref:`MPI_Intercomm_merge`
