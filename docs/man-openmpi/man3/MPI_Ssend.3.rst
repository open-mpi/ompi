.. _mpi_ssend:


MPI_Ssend
=========

.. include_body

:ref:`MPI_Ssend` |mdash| Standard synchronous send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_ssend.rst

INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``count``: Number of elements in send buffer (nonnegative integer).
* ``datatype``: Datatype of each send buffer element (handle).
* ``dest``: Rank of destination (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Ssend` performs a synchronous-mode, blocking send. See the
`MPI Standard <https://www.mpi-forum.org/docs/>`_ for
more detailed information about such sends.


ERRORS
------

.. include:: ./ERRORS.rst
