.. _mpi_pack_size:


MPI_Pack_size
=============

.. include_body

:ref:`MPI_Pack_size` - Returns the upper bound on the amount of space
needed to pack a message.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm,
   	int *size)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PACK_SIZE(INCOUNT, DATATYPE, COMM, SIZE, IERROR)
   	INTEGER	INCOUNT, DATATYPE, COMM, SIZE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Pack_size(incount, datatype, comm, size, ierror)
   	INTEGER, INTENT(IN) :: incount
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``incount``: Count argument to packing call (integer).
* ``datatype``: Datatype argument to packing call (handle).
* ``comm``: Communicator argument to packing call (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Upper bound on size of packed message, in bytes (integer).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Pack_size` allows the application to find out how much space is
needed to pack a message. A call to MPI_Pack_size(incount, datatype,
comm, size) returns in size an upper bound on the increment in position
that would occur in a call to :ref:`MPI_Pack`, with the same values for
*incount*, *datatype*, and *comm*.

**Rationale:** The call returns an upper bound, rather than an exact
bound, since the exact amount of space needed to pack the message may
depend on the context (e.g., first message packed in a packing unit may
take more space).


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
   :ref:`MPI_Pack` :ref:`MPI_Unpack`
