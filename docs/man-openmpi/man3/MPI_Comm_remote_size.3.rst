.. _mpi_comm_remote_size:


MPI_Comm_remote_size
====================

.. include_body

:ref:`MPI_Comm_remote_size` |mdash| Determines the size of the remote group
associated with an intercommunicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_remote_size.rst

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
