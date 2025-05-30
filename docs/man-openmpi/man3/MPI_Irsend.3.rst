.. _mpi_irsend:


MPI_Irsend
==========

.. include_body

:ref:`MPI_Irsend` |mdash| Starts a ready-mode nonblocking send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_irsend.rst

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

:ref:`MPI_Irsend` starts a ready-mode nonblocking send. Nonblocking calls
allocate a communication request object and associate it with the
request handle (the argument request). The request can be used later to
query the status of the communication or to wait for its completion.

A nonblocking send call indicates that the system may start copying data
out of the send buffer. The sender should not modify any part of the
send buffer after a nonblocking send operation is called, until the send
completes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Rsend`
