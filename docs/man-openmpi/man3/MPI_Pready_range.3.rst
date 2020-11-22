.. _mpi_pready_range:


MPI_Pready_range
================

.. include_body

:ref:`MPI_Pready_range` - Indicates that a given range os send-side
partitions are ready to be transferred.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Pready_range(int partition_low, int partition_high, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PREADY(PARTITION_LOW, PARTITION_HIGH, REQUEST, IERROR)
   	INTEGER	PARTITION_LOW, PARTITION_HIGH, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Pready(partition_low, partition_high, request, ierror)
   	INTEGER, INTENT(IN) :: partition_low, partition_high
   	TYPE(MPI_Request), INTENT(IN) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``partition_low``: The lowest of the range of partitions to mark ready for transfer (integer).
* ``partition_high``: The highest of the range of partitions to mark ready for transfer (integer).
* ``request``: Communication request (handle).

OUTPUT PARAMETERS
-----------------
* ``IERROR``: Fortran only: Error status (integer).

ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Pready` :ref:`MPI_Pready_list` :ref:`MPI_Parrived`
