.. _mpi_put:


MPI_Put
=======

.. include_body

:ref:`MPI_Put`, :ref:`MPI_Rput` - Copies data from the origin memory to the
target.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   MPI_Put(const void *origin_addr, int origin_count, MPI_Datatype
   	origin_datatype, int target_rank, MPI_Aint target_disp,
   	int target_count, MPI_Datatype target_datatype, MPI_Win win)

   MPI_Rput(const void *origin_addr, int origin_count, MPI_Datatype
   	 origin_datatype, int target_rank, MPI_Aint target_disp,
   	 int target_count, MPI_Datatype target_datatype, MPI_Win win,
   	 MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PUT(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
   	TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, IERROR)
   	<type> ORIGIN_ADDR(*)
   	INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
   	INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT,
   	TARGET_DATATYPE, WIN, IERROR

   MPI_RPUT(ORIGIN_ADDR, ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK,
   	 TARGET_DISP, TARGET_COUNT, TARGET_DATATYPE, WIN, REQUEST, IERROR)
   	 <type> ORIGIN_ADDR(*)
   	 INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
   	 INTEGER ORIGIN_COUNT, ORIGIN_DATATYPE, TARGET_RANK, TARGET_COUNT,
   	 TARGET_DATATYPE, WIN, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Put(origin_addr, origin_count, origin_datatype, target_rank,
   		target_disp, target_count, target_datatype, win, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
   	INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   	TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Rput(origin_addr, origin_count, origin_datatype, target_rank,
   	target_disp, target_count, target_datatype, win, request,
   		ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
   	INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   	TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``origin_addr``: Initial address of origin buffer (choice).
* ``origin_count``: Number of entries in origin buffer (nonnegative integer).
* ``origin_datatype``: Data type of each entry in origin buffer (handle).
* ``target_rank``: Rank of target (nonnegative integer).
* ``target_disp``: Displacement from start of window to target buffer (nonnegative integer).
* ``target_count``: Number of entries in target buffer (nonnegative integer).
* ``target_datatype``: Data type of each entry in target buffer (handle).
* ``win``: Window object used for communication (handle).

OUTPUT PARAMETER
----------------
* ``request``: MPI_Rput: RMA request
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Put` transfers *origin_count* successive entries of the type
specified by *origin_datatype*, starting at address *origin_addr* on the
origin node to the target node specified by the *win*, *target_rank*

pair. The data are written in the target buffer at address *target_addr*
^ *window_base* + *target_disp* x *disp_unit*, where *window_base* and
*disp_unit* are the base address and window displacement unit specified
at window initialization, by the target process.

The target buffer is specified by the arguments *target_count* and
*target_datatype*.

The data transfer is the same as that which would occur if the origin
process executed a send operation with arguments *origin_addr*,
*origin_count*, *origin_datatype*, *target_rank*, *tag*, *comm*, and the
target process executed a receive operation with arguments
*target_addr*, *target_count*, *target_datatype*, *source*, *tag*,
*comm*, where *target_addr* is the target buffer address computed as
explained above, and *comm* is a communicator for the group of *win*.

The communication must satisfy the same constraints as for a similar
message-passing communication. The *target_datatype* may not specify
overlapping entries in the target buffer. The message sent must fit,
without truncation, in the target buffer. Furthermore, the target buffer
must fit in the target window. In addition, only processes within the
same buffer can access the target window.

The *target_datatype* argument is a handle to a datatype object defined
at the origin process. However, this object is interpreted at the target
process: The outcome is as if the target datatype object were defined at
the target process, by the same sequence of calls used to define it at
the origin process. The target data type must contain only relative
displacements, not absolute addresses. The same holds for get and
accumulate.

:ref:`MPI_Rput` is similar to :ref:`MPI_Put`, except that it allocates a
communication request object and associates it with the request handle
(the argument *request*). The completion of an :ref:`MPI_Rput` operation (i.e.,
after the corresponding test or wait) indicates that the sender is now
free to update the locations in the *origin_addr* buffer. It does not
indicate that the data is available at the target window. If remote
completion is required, :ref:`MPI_Win_flush`, :ref:`MPI_Win_flush_all`,
:ref:`MPI_Win_unlock`, or :ref:`MPI_Win_unlock_all` can be used.


NOTES
-----

The *target_datatype* argument is a handle to a datatype object that is
defined at the origin process, even though it defines a data layout in
the target process memory. This does not cause problems in a homogeneous
or heterogeneous environment, as long as only portable data types are
used (portable data types are defined in Section 2.4 of the MPI-2
Standard).

The performance of a put transfer can be significantly affected, on some
systems, from the choice of window location and the shape and location
of the origin and target buffer: Transfers to a target window in memory
allocated by :ref:`MPI_Alloc_mem` may be much faster on shared memory systems;
transfers from contiguous buffers will be faster on most, if not all,
systems; the alignment of the communication buffers may also impact
performance.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Get`
   * :ref:`MPI_Rget`
   * :ref:`MPI_Accumulate`
   * :ref:`MPI_Win_flush`
   * :ref:`MPI_Win_flush_all`
   * :ref:`MPI_Win_unlock`
   * :ref:`MPI_Win_unlock_all`
