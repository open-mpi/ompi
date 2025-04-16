.. _mpi_bsend_init:

MPI_Bsend_init
==============

.. include_body

:ref:`MPI_Bsend_init` |mdash| Builds a handle for a buffered send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_bsend_init.rst

INPUT PARAMETERS
----------------

* ``buf`` : Initial address of send buffer (choice).
* ``count`` : Number of elements sent (integer).
* ``datatype`` : Type of each element (handle).
* ``dest`` : Rank of destination (integer).
* ``tag`` : Message tag (integer).
* ``comm`` : Communicator (handle).

OUTPUT PARAMETERS
-----------------

* ``request`` : Communication request (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Creates a persistent communication request for a buffered mode send, and
binds to it all the arguments of a send operation.

A communication (send or receive) that uses a persistent request is
initiated by the function :ref:`MPI_Start`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Send_init` :ref:`MPI_Start`
