.. _mpi_cart_map:

MPI_Cart_map
============

.. include_body

:ref:`MPI_Cart_map` |mdash| Maps process to Cartesian topology information.

.. The following file was automatically generated
.. include:: ./bindings/mpi_cart_map.rst

INPUT PARAMETERS
----------------

* ``comm`` : Input communicator (handle).
* ``ndims`` : Number of dimensions of Cartesian structure (integer).
* ``dims`` : Integer array of size ndims specifying the number of processes
   in each coordinate direction.
* ``periods`` : Logical array of size ndims specifying the periodicity
   specification in each coordinate direction.

OUTPUT PARAMETERS
-----------------

* ``newrank`` : Reordered rank of the calling process; MPI_UNDEFINED if
   calling process does not belong to grid (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Cart_map` and :ref:`MPI_Graph_map` can be used to implement all other
topology functions. In general they will not be called by the user
directly, unless he or she is creating additional virtual topology
capability other than that provided by MPI. :ref:`MPI_Cart_map` computes an
"optimal" placement for the calling process on the physical machine. A
possible implementation of this function is to always return the rank of
the calling process, that is, not to perform any reordering.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Graph_map`
