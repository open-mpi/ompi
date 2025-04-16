.. _mpi_cart_get:

MPI_Cart_get
============

.. include_body

:ref:`MPI_Cart_get` |mdash| Retrieves Cartesian topology information associated with
a communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_cart_get.rst

INPUT PARAMETERS
----------------

* ``comm`` : Communicator with Cartesian structure (handle).
* ``maxdims`` : Length of vectors dims, periods, and coords in the calling
   program (integer).

OUTPUT PARAMETERS
-----------------

* ``dims`` : Number of processes for each Cartesian dimension (array of
   integers).
* ``periods`` : Periodicity (true/false) for each Cartesian dimension
   (array of logicals).
* ``coords`` : Coordinates of calling process in Cartesian structure (array
   of integers).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

The functions :ref:`MPI_Cartdim_get` and :ref:`MPI_Cart_get` return the Cartesian
topology information that was associated with a communicator by
:ref:`MPI_Cart_create`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Cartdim_get`
