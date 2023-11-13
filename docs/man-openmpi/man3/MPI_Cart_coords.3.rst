.. _mpi_cart_coords:

MPI_Cart_coords
===============

.. include_body

:ref:`MPI_Cart_coords` |mdash| Determines process coords in Cartesian topology
given rank in group.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims,
       int coords[])

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CART_COORDS(COMM, RANK, MAXDIMS, COORDS, IERROR)
       INTEGER COMM, RANK, MAXDIMS, COORDS(*), IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Cart_coords(comm, rank, maxdims, coords, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: rank, maxdims
       INTEGER, INTENT(OUT) :: coords(maxdims)
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
