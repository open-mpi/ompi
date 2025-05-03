.. _mpi_isend:


MPI_Isend
=========

.. include_body

:ref:`MPI_Isend` |mdash| Starts a standard-mode, nonblocking send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_isend.rst

INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``count``: Number of elements in send buffer (integer).
* ``datatype``: Datatype of each send buffer element (handle).
* ``dest``: Rank of destination (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Isend` starts a standard-mode, nonblocking send. Nonblocking calls
allocate a communication request object and associate it with the
request handle (the argument request). The request can be used later to
query the status of the communication or wait for its completion.

A nonblocking send call indicates that the system may start copying data
out of the send buffer. The sender should not modify any part of the
send buffer after a nonblocking send operation is called, until the send
completes.

A send request can be determined being completed by calling the
:ref:`MPI_Wait`, :ref:`MPI_Waitany`, :ref:`MPI_Test`, or :ref:`MPI_Testany` with request returned by
this function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Send`
   * :ref:`MPI_Wait`
   * :ref:`MPI_Waitany`
   * :ref:`MPI_Test`
   * :ref:`MPI_Testany`
