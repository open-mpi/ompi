.. _mpi_irecv:


MPI_Irecv
=========

.. include_body

:ref:`MPI_Irecv` |mdash| Starts a standard-mode, nonblocking receive.

.. The following file was automatically generated
.. include:: ./bindings/mpi_irecv.rst

INPUT PARAMETERS
----------------
* ``buf``: Initial address of receive buffer (choice).
* ``count``: Number of elements in receive buffer (integer).
* ``datatype``: Datatype of each receive buffer element (handle).
* ``source``: Rank of source (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Nonblocking calls allocate a communication request object and associate
it with the request handle (the argument request). The request can be
used later to query the status of the communication or wait for its
completion.

A nonblocking receive call indicates that the system may start writing
data into the receive buffer. The receiver should not access any part of
the receive buffer after a nonblocking receive operation is called,
until the receive completes.

A receive request can be determined being completed by calling the
:ref:`MPI_Wait`, :ref:`MPI_Waitany`, :ref:`MPI_Test`, or :ref:`MPI_Testany` with request returned by
this function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Recv`
   * :ref:`MPI_Probe`
   * :ref:`MPI_Test`
   * :ref:`MPI_Testany`
   * :ref:`MPI_Wait`
   * :ref:`MPI_Waitany`
