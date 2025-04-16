.. _mpi_rsend:


MPI_Rsend
=========

.. include_body

:ref:`MPI_Rsend` |mdash| Ready send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_rsend.rst

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

A ready send may only be called if the user can guarantee that a receive
is already posted. It is an error if the receive is not posted before
the ready send is called.


ERRORS
------

.. include:: ./ERRORS.rst
