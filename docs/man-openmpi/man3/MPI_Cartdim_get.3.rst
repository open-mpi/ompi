.. _mpi_cartdim_get:

MPI_Cartdim_get
===============

.. include_body

:ref:`MPI_Cartdim_get` |mdash| Retrieves Cartesian topology information associated
with a communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_cartdim_get.rst

INPUT PARAMETER
---------------

* ``comm`` : Communicator with Cartesian structure (handle).

OUTPUT PARAMETERS
-----------------

* ``ndims`` : Number of dimensions of the Cartesian structure (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Cartdim_get` returns the number of dimensions of the Cartesian
structure.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Cart_get`
