.. _mpi_cart_coords:

MPI_Cart_coords
===============

.. include_body

:ref:`MPI_Cart_coords` - Determines process coords in Cartesian topology
given rank in group.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims,
       int coords[])

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CART_COORDS(COMM, RANK, MAXDIMS, COORDS, IERROR)
       INTEGER COMM, RANK, MAXDIMS, COORDS(*), IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Cart_coords(comm, rank, maxdims, coords, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: rank, maxdims
       INTEGER, INTENT(OUT) :: coords(maxdims)
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  ``comm`` : Communicator with Cartesian structure (handle).
-  ``rank`` : Rank of a process within group of comm (integer).
-  ``maxdims`` : Length of vector coords in the calling program
   (integer). Length of vector coords in the calling program (integer).

Output Parameters
-----------------

-  ``coords`` : Integer array (of size ndims,which was defined by
   :ref:`MPI_Cart_create` call) containing the Cartesian coordinates of
   specified process (integer).
-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Cart_coords` provies a mapping of ``rank``\ s to Cartesian
coordinates.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
