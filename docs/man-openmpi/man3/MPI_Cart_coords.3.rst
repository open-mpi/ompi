.. _mpi_cart_coords:

MPI_Cart_coords
===============

.. include_body

:ref:`MPI_Cart_coords` |mdash| Determines process coords in Cartesian topology
given rank in group.

.. The following file was automatically generated
.. include:: ./bindings/mpi_cart_coords.rst

INPUT PARAMETERS
----------------

* ``comm`` : Communicator with Cartesian structure (handle).
* ``rank`` : Rank of a process within group of comm (integer).
* ``maxdims`` : Length of vector coords in the calling program
   (integer). Length of vector coords in the calling program (integer).

OUTPUT PARAMETERS
-----------------

* ``coords`` : Integer array (of size ndims,which was defined by
   :ref:`MPI_Cart_create` call) containing the Cartesian coordinates of
   specified process (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Cart_coords` provides a mapping of ``rank``\ s to Cartesian
coordinates.

ERRORS
------

.. include:: ./ERRORS.rst
