.. _mpi_cart_rank:

MPI_Cart_rank
=============

.. include_body

:ref:`MPI_Cart_rank` |mdash| Determines process rank in communicator given Cartesian
location.

.. The following file was automatically generated
.. include:: ./bindings/mpi_cart_rank.rst

INPUT PARAMETERS
----------------

* ``comm`` : Communicator with Cartesian structure (handle).
* ``coords`` : Integer array (of size ndims, which was defined by
   :ref:`MPI_Cart_create` call) specifying the Cartesian coordinates of a
   process.

OUTPUT PARAMETER
----------------

* ``rank`` : Rank of specified process (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

For a process group with Cartesian structure, the function :ref:`MPI_Cart_rank`
translates the logical process coordinates to process ranks as they are
used by the point-to-point routines. For dimension i with periods(i) =
true, if the coordinate, coords(i), is out of range, that is, coords(i)
< 0 or coords(i) >= dims(i), it is shifted back to the interval 0 =<
coords(i) < dims(i) automatically. Out-of-range coordinates are
erroneous for nonperiodic dimensions.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Cart_create`
