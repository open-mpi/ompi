.. _mpi_comm_group:


MPI_Comm_group
==============

.. include_body

:ref:`MPI_Comm_group` |mdash| Returns the group associated with a communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_group.rst

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
