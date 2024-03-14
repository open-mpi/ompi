.. _mpi_comm_group:


MPI_Comm_group
==============

.. include_body

:ref:`MPI_Comm_group` |mdash| Returns the group associated with a communicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_group(MPI_Comm comm, MPI_Group *group)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_GROUP(COMM, GROUP, IERROR)
     	INTEGER	COMM, GROUP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_group(comm, group, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Group), INTENT(OUT) :: group
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator.

OUTPUT PARAMETERS
-----------------
* ``group``: Group in communicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

If the communicator is an intercommunicator (enables communication
between two groups of processes), this function returns the local group.
To return the remote group, use the :ref:`MPI_Comm_remote_group` function.


ERRORS
------

.. include:: ./ERRORS.rst
