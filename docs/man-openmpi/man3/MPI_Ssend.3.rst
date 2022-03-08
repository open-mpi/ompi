.. _mpi_ssend:


MPI_Ssend
=========

.. include_body

:ref:`MPI_Ssend` - Standard synchronous send.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Ssend(const void *buf, int count, MPI_Datatype datatype, int dest,
   	int tag, MPI_Comm comm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SSEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, DEST, TAG, COMM, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Ssend(buf, count, datatype, dest, tag, comm, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: buf
   	INTEGER, INTENT(IN) :: count, dest, tag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``count``: Number of elements in send buffer (nonnegative integer).
* ``datatype``: Datatype of each send buffer element (handle).
* ``dest``: Rank of destination (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Ssend` performs a synchronous-mode, blocking send. See the MPI-1
Standard for more detailed information about such sends.


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
