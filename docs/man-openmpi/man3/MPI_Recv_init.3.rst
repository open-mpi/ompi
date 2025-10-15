.. _mpi_recv_init:


MPI_Recv_init
=============

.. include_body

:ref:`MPI_Recv_init` |mdash| Builds a handle for a receive.

.. The following file was automatically generated
.. include:: ./bindings/mpi_recv_init.rst

INPUT PARAMETERS
----------------
* ``count``: Maximum number of elements to receive (integer).
* ``datatype``: Type of each entry (handle).
* ``source``: Rank of source (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

INPUT/OUTPUT PARAMETER
----------------------
* ``buf``: Initial address of receive buffer (choice).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Creates a persistent communication request for a receive operation. The
argument *buf* is marked as OUT because the user gives permission to
write on the receive buffer by passing the argument to :ref:`MPI_Recv_init`.

A persistent communication request is inactive after it is created |mdash| no
active communication is attached to the request.

A communication (send or receive) that uses a persistent request is
initiated by the function :ref:`MPI_Start` or :ref:`MPI_Startall`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Bsend_init`
   * :ref:`MPI_Rsend_init`
   * :ref:`MPI_Send_init`
   * :ref:`MPI_Ssend_init`
   * :ref:`MPI_Start`
   * :ref:`MPI_Startall`
   * :ref:`MPI_Request_free`
