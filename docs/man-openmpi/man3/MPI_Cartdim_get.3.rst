.. _mpi_cartdim_get:

MPI_Cartdim_get
===============

.. include_body

:ref:`MPI_Cartdim_get` - Retrieves Cartesian topology information associated
with a communicator.

Syntax
------

C Syntax
^^^^^^^^

.. code:: C

   #include <mpi.h>

   int MPI_Cartdim_get(MPI_Comm comm, int *ndims)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CARTDIM_GET(COMM, NDIMS, IERROR)
       INTEGER COMM, NDIMS, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Cartdim_get(comm, ndims, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(OUT) :: ndims
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  comm : Communicator with Cartesian structure (handle).

Output Parameters
-----------------

-  ndims : Number of dimensions of the Cartesian structure (integer).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Cartdim_get` returns the number of dimensions of the Cartesian
structure.

Errors
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Cart_get`
