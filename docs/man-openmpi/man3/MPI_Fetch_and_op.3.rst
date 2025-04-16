.. _mpi_fetch_and_op:


MPI_Fetch_and_op
================

.. include_body

:ref:`MPI_Fetch_and_op` |mdash| Combines the contents of the origin buffer with
that of a target buffer and returns the target buffer value.

.. The following file was automatically generated
.. include:: ./bindings/mpi_fetch_and_op.rst

INPUT PARAMETERS
----------------
* ``origin_addr``: Initial address of buffer (choice).
* ``result_addr``: Initial address of result buffer (choice).
* ``datatype``: Data type of the entry in origin, result, and target buffers (handle).
* ``target_rank``: Rank of target (nonnegative integer).
* ``target_disp``: Displacement from start of window to beginning of target buffer (nonnegative integer).
* ``op``: Reduce operation (handle).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Accumulate one element of type *datatype* from the origin buffer
(*origin_addr*) to the buffer at offset *target_disp*, in the target
window specified by *target_rank* and *win*, using the operation *op*
and return in the result buffer *result_addr* the contents of the target
buffer before the accumulation.

The origin and result buffers (*origin_addr* and *result_addr*) must be
disjoint. Any of the predefined operations for MPI_Rreduce, as well
as MPI_NO_OP or MPI_REPLACE, can be specified as *op*; user-defined
functions cannot be used. The *datatype* argument must be a predefined
datatype. The operation is executed atomically.

A new predefined operation, MPI_REPLACE, is defined. It corresponds to
the associative function f(a, b) =b; that is, the current value in the
target memory is replaced by the value supplied by the origin.

A new predefined operation, MPI_NO_OP, is defined. It corresponds to the
assiciative function f(a, b) = a; that is the current value in the
target memory is returned in the result buffer at the origin and no
operation is performed on the target buffer.


NOTES
-----

It is the user's responsibility to guarantee that, when using the
accumulate functions, the target displacement argument is such that
accesses to the window are properly aligned according to the data type
arguments in the call to the :ref:`MPI_Fetch_and_op` function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Get_accumulate`
   * :ref:`MPI_Reduce`
