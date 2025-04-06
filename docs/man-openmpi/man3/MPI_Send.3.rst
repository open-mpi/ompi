.. _mpi_send:


MPI_Send
========

.. include_body

:ref:`MPI_Send` |mdash| Performs a standard-mode blocking send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_send.rst

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

:ref:`MPI_Send` performs a standard-mode, blocking send.


NOTE
----

This routine will block until the message is sent to the destination.
For an in-depth explanation of the semantics of the standard-mode send,
refer to the `MPI Standard <https://www.mpi-forum.org/docs/>`_.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Isend`
   * :ref:`MPI_Bsend`
   * :ref:`MPI_Recv`
