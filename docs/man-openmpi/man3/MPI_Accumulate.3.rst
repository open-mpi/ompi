.. _mpi_accumulate:


MPI_Accumulate
==============

.. include_body

:ref:`MPI_Accumulate`, :ref:`MPI_Raccumulate` - Combines the contents of the
origin buffer with that of a target buffer.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Accumulate(const void *origin_addr, int origin_count,
   	MPI_Datatype origin_datatype, int target_rank,
   	MPI_Aint target_disp, int target_count,
   	MPI_Datatype target_datatype, MPI_Op op, MPI_Win win)

   int MPI_Raccumulate(const void *origin_addr, int origin_count,
   	MPI_Datatype origin_datatype, int target_rank,
   	MPI_Aint target_disp, int target_count,
   	MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
   	MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ACCUMULATE(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
   	TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, OP, WIN, IERROR)
   	<type> ORIGIN_ADDR(*)
   	INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
   	INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT,
   	TARGET_DATATYPE, OP, WIN, IERROR

   MPI_RACCUMULATE(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
   	TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, OP, WIN, REQUEST, IERROR)
   	<type> ORIGIN_ADDR(*)
   	INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
   	INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT,
   	TARGET_DATATYPE, OP, WIN, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Accumulate(origin_addr, origin_count, origin_datatype, target_rank,
   		target_disp, target_count, target_datatype, op, win, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
   	INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   	TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Raccumulate(origin_addr, origin_count, origin_datatype, target_rank,
   	target_disp, target_count, target_datatype, op, win, request,
   		ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
   	INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   	TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``origin_addr``: Initial address of buffer (choice).
* ``origin_count``: Number of entries in buffer (nonnegative integer).
* ``origin_datatype``: Data type of each buffer entry (handle).
* ``target_rank``: Rank of target (nonnegative integer).
* ``target_disp``: Displacement from start of window to beginning of target buffer (nonnegative integer).
* ``target_count``: Number of entries in target buffer (nonnegative integer).
* ``target_datatype``: Data type of each entry in target buffer (handle).
* ``op``: Reduce operation (handle).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``MPI_Raccumulate``:  RMA request
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Accumulate` is a function used for one-sided MPI communication
that adds the contents of the origin buffer (as defined by
*origin_addr*, *origin_count*, and *origin_datatype*) to the buffer
specified by the arguments *target_count* and *target_datatype*, at
offset *target_disp*, in the target window specified by *target_rank*
and *win*, using the operation *op*. The target window can only be
accessed by processes within the same node. This is similar to :ref:`MPI_Put`,
except that data is combined into the target area instead of overwriting
it.

Any of the predefined operations for :ref:`MPI_Reduce` can be used.
User-defined functions cannot be used. For example, if *op* is MPI_SUM,
each element of the origin buffer is added to the corresponding element
in the target, replacing the former value in the target.

Each datatype argument must be a predefined data type or a derived data
type, where all basic components are of the same predefined data type.
Both datatype arguments must be constructed from the same predefined
data type. The operation *op* applies to elements of that predefined
type. The *target_datatype* argument must not specify overlapping
entries, and the target buffer must fit in the target window.

A new predefined operation, MPI_REPLACE, is defined. It corresponds to
the associative function f(a, b) =b; that is, the current value in the
target memory is replaced by the value supplied by the origin.

:ref:`MPI_Raccumulate` is similar to :ref:`MPI_Accumulate`, except that it
allocates a communication request object and associates it with the
request handle (the argument *request*) that can be used to wait or test
for completion. The completion of an :ref:`MPI_Raccumulate` operation
indicates that the *origin_addr* buffer is free to be updated. It does
not indicate that the operation has completed at the target window.


NOTES
-----

:ref:`MPI_Put` is a special case of :ref:`MPI_Accumulate`, with the operation
MPI_REPLACE. Note, however, that :ref:`MPI_Put` and :ref:`MPI_Accumulate` have
different constraints on concurrent updates.

It is the user's responsibility to guarantee that, when using the
accumulate functions, the target displacement argument is such that
accesses to the window are properly aligned according to the data type
arguments in the call to the :ref:`MPI_Accumulate` function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Put`
   * :ref:`MPI_Get_accumulate`
   * :ref:`MPI_Reduce`
