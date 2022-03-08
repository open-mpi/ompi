.. _mpi_parrived:


MPI_Parrived
============

.. include_body

:ref:`MPI_Parrived` - Tests for completion of a specified receive-side
partition.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Parrived(MPI_Request *request, int partition, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PARRIVED(REQUEST, PARTITION, FLAG IERROR)
   	INTEGER	REQUEST, PARTITION, FLAG(*), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Parrived(request, partition, flag, ierror)
   	TYPE(MPI_Request), INTENT(in) :: request
           INTEGER, INTENT(IN) :: partition
           INTEGER, INTENT(out) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``request``: Communication request (handle).
* ``partition``: The number of the partition to test for completion (integer).

OUTPUT PARAMETERS
-----------------
* ``flag``: True if partition is completed.
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
   :ref:`MPI_Pready_list` :ref:`MPI_Pready_range` :ref:`MPI_Parrived`
