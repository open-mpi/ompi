.. _mpi_accumulate:


MPI_Accumulate
==============

.. include_body

:ref:`MPI_Accumulate`, :ref:`MPI_Raccumulate` - Combines the contents of the
origin buffer with that of a target buffer.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Accumulate, MPI_Raccumulate

.. The following file was automatically generated
.. include:: ./bindings/mpi_accumulate.rst

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
