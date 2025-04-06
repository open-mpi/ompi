.. _mpi_comm_remote_group:


MPI_Comm_remote_group
=====================

.. include_body

:ref:`MPI_Comm_remote_group` |mdash| Accesses the remote group associated with an
intercommunicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_remote_group.rst

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
