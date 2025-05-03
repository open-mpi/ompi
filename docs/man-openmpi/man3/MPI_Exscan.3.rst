.. _mpi_exscan:


MPI_Exscan
==========

.. include_body

:ref:`MPI_Exscan`, :ref:`MPI_Iexscan`, :ref:`MPI_Exscan_init` - Computes an exclusive scan (partial
reduction)

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Exscan, MPI_Iexscan, MPI_Exscan_init

.. The following file was automatically generated
.. include:: ./bindings/mpi_exscan.rst

INPUT PARAMETERS
----------------
* ``sendbuf``: Send buffer (choice).
* ``count``: Number of elements in input buffer (integer).
* ``datatype``: Data type of elements of input buffer (handle).
* ``op``: Operation (handle).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Receive buffer (choice).
* ``request``: Request (handle, non-blocking and persistent only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Exscan` is used to perform an exclusive prefix reduction on data
distributed across the calling processes. The operation returns, in the
*recvbuf* of the process with rank i, the reduction (calculated
according to the function *op*) of the values in the *sendbuf*\ s of
processes with ranks 0, ..., i-1. Compare this with the functionality of
:ref:`MPI_Scan`, which calculates over the range 0, ..., i (inclusive). The
type of operations supported, their semantics, and the constraints on
send and receive buffers are as for :ref:`MPI_Reduce`.

The value in *recvbuf* on process 0 is undefined and unreliable as
*recvbuf* is not significant for process 0. The value of *recvbuf* on
process 1 is always the value in *sendbuf* on process 0.


USE OF IN-PLACE OPTION
----------------------

The \`in place' option for intracommunicators is specified by passing
MPI_IN_PLACE in the *sendbuf* argument. In this case, the input data is
taken from the receive buffer, and replaced by the output data.

Note that MPI_IN_PLACE is a special kind of value; it has the same
restrictions on its use as MPI_BOTTOM.

Because the in-place option converts the receive buffer into a
send-and-receive buffer, a Fortran binding that includes INTENT must
mark these as INOUT, not OUT.


NOTES
-----

MPI does not specify which process computes which operation. In
particular, both processes 0 and 1 may participate in the computation
even though the results for both processes' *recvbuf* are degenerate.
Therefore, all processes, including 0 and 1, must provide the same *op*.

It can be argued, from a mathematical perspective, that the definition
of :ref:`MPI_Exscan` is unsatisfactory because the output at process 0 is
undefined. The "mathematically correct" output for process 0 would be
the unit element of the reduction operation. However, such a definition
of an exclusive scan would not work with user-defined *op* functions as
there is no way for MPI to "know" the unit value for these custom
operations.


NOTES ON COLLECTIVE OPERATIONS
------------------------------

The reduction functions of type MPI_Op do not return an error value. As
a result, if the functions detect an error, all they can do is either
call :ref:`MPI_Abort` or silently skip the problem. Thus, if the error handler
is changed from MPI_ERRORS_ARE_FATAL to something else (e.g.,
MPI_ERRORS_RETURN), then no error may be indicated.

The reason for this is the performance problems in ensuring that all
collective routines return the same error value.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Op_create`
   * :ref:`MPI_Reduce`
   * :ref:`MPI_Scan`
