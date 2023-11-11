.. _mpi_cartdim_get:

MPI_Cartdim_get
===============

.. include_body

:ref:`MPI_Cartdim_get` |mdash| Retrieves Cartesian topology information associated
with a communicator.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Cartdim_get(MPI_Comm comm, int *ndims)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CARTDIM_GET(COMM, NDIMS, IERROR)
       INTEGER COMM, NDIMS, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Cartdim_get(comm, ndims, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(OUT) :: ndims
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
