.. _mpi_cart_rank:

MPI_Cart_rank
=============

.. include_body

:ref:`MPI_Cart_rank` - Determines process rank in communicator given Cartesian
location.

Syntax
------

C Syntax
^^^^^^^^

.. code:: C

   #include <mpi.h>

   int MPI_Cart_rank(MPI_Comm comm, int coords[], int *rank)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CART_RANK(COMM, COORDS, RANK, IERROR)
       INTEGER COMM, COORDS(*), RANK, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: Fortran

   USE mpi_f08

   MPI_Cart_rank(comm, coords, rank, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: coords(*)
       INTEGER, INTENT(OUT) :: rank
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  comm : Communicator with Cartesian structure (handle).
-  coords : Integer array (of size ndims, which was defined by
   :ref:`MPI_Cart_create` call) specifying the Cartesian coordinates of a
   process.

Output Parameter
----------------

-  rank : Rank of specified process (integer).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

For a process group with Cartesian structure, the function :ref:`MPI_Cart_rank`
translates the logical process coordinates to process ranks as they are
used by the point-to-point routines. For dimension i with periods(i) =
true, if the coordinate, coords(i), is out of range, that is, coords(i)
< 0 or coords(i) >= dims(i), it is shifted back to the interval 0 =<
coords(i) < dims(i) automatically. Out-of-range coordinates are
erroneous for nonperiodic dimensions.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with :ref:`MPI_Comm_set_errhandler`;
the predefined error handler MPI_ERRORS_RETURN may be used to cause
error values to be returned. Note that MPI does not guarantee that an
MPI program can continue past an error.


.. seealso:: :ref:`MPI_Cart_create`
