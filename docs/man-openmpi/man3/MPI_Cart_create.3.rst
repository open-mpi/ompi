.. _mpi_cart_create:

MPI_Cart_create
===============

.. include_body

:ref:`MPI_Cart_create` - Makes a new communicator to which Cartesian
topology information has been attached.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Cart_create(MPI_Comm comm_old, int ndims, const int dims[],

       const int periods[], int reorder, MPI_Comm *comm_cart)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CART_CREATE(COMM_OLD, NDIMS, DIMS, PERIODS, REORDER,
           COMM_CART, IERROR)
       INTEGER COMM_OLD, NDIMS, DIMS(*), COMM_CART, IERROR
       LOGICAL PERIODS(*), REORDER

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Cart_create(comm_old, ndims, dims, periods, reorder, comm_cart, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm_old
       INTEGER, INTENT(IN) :: ndims, dims(ndims)
       LOGICAL, INTENT(IN) :: periods(ndims), reorder
       TYPE(MPI_Comm), INTENT(OUT) :: comm_cart
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  ``comm_old`` : Input communicator (handle).
-  ``ndims`` : Number of dimensions of Cartesian grid (integer).
-  ``dims`` : Integer array of size ndims specifying the number of
   processes in each dimension.
-  ``periods`` : Logical array of size ndims specifying whether the grid
   is periodic (true) or not (false) in each dimension.
-  ``reorder`` : Ranking may be reordered (true) or not (false)
   (logical).

Output Parameters
-----------------

-  ``comm_cart`` : Communicator with new Cartesian topology (handle).
-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Cart_create` returns a handle to a new communicator to which the
Cartesian topology information is attached. If ``reorder`` = false then
the rank of each process in the new group is identical to its rank in
the old group. Otherwise, the function may ``reorder`` the processes
(possibly so as to choose a good embedding of the virtual topology onto
the physical machine). If the total size of the Cartesian grid is
smaller than the size of the group of comm, then some processes are
returned MPI_COMM_NULL, in analogy to :ref:`MPI_Comm_split`. The call
is erroneous if it specifies a grid that is larger than the group size.

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
