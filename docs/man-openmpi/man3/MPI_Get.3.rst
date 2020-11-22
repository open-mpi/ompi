.. _mpi_get:

MPI_Get
=======

.. include_body

:ref:`MPI_Get`, :ref:`MPI_Rget` - Copies data from the target memory to the origin.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   MPI_Get(void *origin_addr, int origin_count, MPI_Datatype
       origin_datatype, int target_rank, MPI_Aint target_disp,
       int target_count, MPI_Datatype target_datatype, MPI_Win win)

   MPI_Rget(void *origin_addr, int origin_count, MPI_Datatype
        origin_datatype, int target_rank, MPI_Aint target_disp,
        int target_count, MPI_Datatype target_datatype, MPI_Win win,
            MPI_Request *request)

Fortran Syntax (See Fortran 77 Notes)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
       TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, IERROR)
       <type> ORIGIN_ADDR(*)
       INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
       INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
       TARGET_COUNT, TARGET_DATATYPE, WIN, IERROR

   MPI_RGET(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
        TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, REQUEST, IERROR)
        <type> ORIGIN_ADDR(*)
        INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
        INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
        TARGET_COUNT, TARGET_DATATYPE, WIN, REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Get(origin_addr, origin_count, origin_datatype, target_rank,
           target_disp, target_count, target_datatype, win, ierror)
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: origin_addr
       INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
       TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
       INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
       TYPE(MPI_Win), INTENT(IN) :: win
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Rget(origin_addr, origin_count, origin_datatype, target_rank,
       target_disp, target_count, target_datatype, win, request,
           ierror)
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: origin_addr
       INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
       TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
       INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
       TYPE(MPI_Win), INTENT(IN) :: win
       TYPE(MPI_Request), INTENT(OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  origin_addr : Initial address of origin buffer (choice).
-  origin_count : Number of entries in origin buffer (nonnegative
   integer).
-  origin_datatype : Data type of each entry in origin buffer (handle).
-  target_rank : Rank of target (nonnegative integer).
-  target_disp : Displacement from window start to the beginning of the
   target buffer (nonnegative integer).
-  target_count : Number of entries in target buffer (nonnegative
   integer).
-  target datatype : datatype of each entry in target buffer (handle)
-  win : window object used for communication (handle)

Output Parameter
----------------

-  request : :ref:`MPI_Rget`: RMA request
-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Get` copies data from the target memory to the origin, similar to
:ref:`MPI_Put`, except that the direction of data transfer is reversed. The
origin_datatype may not specify overlapping entries in the origin
buffer. The target buffer must be contained within the target window,
and the copied data must fit, without truncation, in the origin buffer.
Only processes within the same node can access the target window.

:ref:`MPI_Rget` is similar to :ref:`MPI_Get`, except that it allocates a communication
request object and associates it with the request handle (the argument
request) that can be used to wait or test for completion. The completion
of an :ref:`MPI_Rget` operation indicates that the data is available in the
origin buffer. If origin_addr points to memory attached to a window,
then the data becomes available in the private copy of this window.

Fortran 77 Notes
----------------

The MPI standard prescribes portable Fortran syntax for the TARGET_DISP
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax

fortran INTEGERMPI_ADDRESS_KIND TARGET_DISP

where MPI_ADDRESS_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso:: :ref:`MPI_Put`
