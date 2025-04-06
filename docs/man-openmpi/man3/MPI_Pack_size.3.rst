.. _mpi_pack_size:


MPI_Pack_size
=============

.. include_body

:ref:`MPI_Pack_size` |mdash| Returns the upper bound on the amount of space
needed to pack a message.

.. The following file was automatically generated
.. include:: ./bindings/mpi_pack_size.rst

INPUT PARAMETERS
----------------
* ``incount``: Count argument to packing call (integer).
* ``datatype``: Datatype argument to packing call (handle).
* ``comm``: Communicator argument to packing call (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Upper bound on size of packed message, in bytes (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Pack_size` allows the application to find out how much space is
needed to pack a message. A call to MPI_Pack_size(incount, datatype,
comm, size) returns in size an upper bound on the increment in position
that would occur in a call to :ref:`MPI_Pack`, with the same values for
*incount*, *datatype*, and *comm*.

**Rationale:** The call returns an upper bound, rather than an exact
bound, since the exact amount of space needed to pack the message may
depend on the context (e.g., first message packed in a packing unit may
take more space).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pack`
   * :ref:`MPI_Unpack`
