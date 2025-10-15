.. _mpi_rsend_init:


MPI_Rsend_init
==============

.. include_body

:ref:`MPI_Rsend_init` |mdash| Builds a handle for a ready send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_rsend_init.rst

INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``count``: Number of elements sent (integer).
* ``datatype``: Type of each element (handle).
* ``dest``: Rank of destination (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Creates a persistent communication object for a ready mode send
operation, and binds to it all the arguments of a send operation.

A communication (send or receive) that uses a persistent request is
initiated by the function :ref:`MPI_Start`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Bsend_init`
   * :ref:`MPI_Send_init`
   * :ref:`MPI_Ssend_init`
   * :ref:`MPI_Recv_init`
   * :ref:`MPI_Start`
   * :ref:`MPI_Startall`
   * :ref:`MPI_Request_free`
