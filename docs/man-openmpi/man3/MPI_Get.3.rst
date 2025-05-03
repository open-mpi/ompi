.. _mpi_get:

MPI_Get
=======

.. include_body

:ref:`MPI_Get`, :ref:`MPI_Rget` - Copies data from the target memory to the origin.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Get, MPI_Rget

.. The following file was automatically generated
.. include:: ./bindings/mpi_get.rst

INPUT PARAMETERS
----------------

* ``origin_addr`` : Initial address of origin buffer (choice).
* ``origin_count`` : Number of entries in origin buffer (nonnegative
   integer).
* ``origin_datatype`` : Data type of each entry in origin buffer (handle).
* ``target_rank`` : Rank of target (nonnegative integer).
* ``target_disp`` : Displacement from window start to the beginning of the
   target buffer (nonnegative integer).
* ``target_count`` : Number of entries in target buffer (nonnegative
   integer).
* target datatype : datatype of each entry in target buffer (handle)
* ``win`` : window object used for communication (handle)

OUTPUT PARAMETER
----------------

* ``request`` : :ref:`MPI_Rget`: RMA request
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
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


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Put`
