.. _mpi_get_count:

MPI_Get_count
=============

.. include_body

:ref:`MPI_Get_count` |mdash| Gets the number of top-level elements received.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Get_count(const MPI_Status *status, MPI_Datatype datatype,
       int *count)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_COUNT(STATUS, DATATYPE, COUNT, IERROR)
       INTEGER STATUS(MPI_STATUS_SIZE), DATATYPE, COUNT, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Get_count(status, datatype, count, ierror)
       TYPE(MPI_Status), INTENT(IN) :: status
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       INTEGER, INTENT(OUT) :: count
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``status`` : Return status of receive operation (status).
* ``datatype`` : Datatype of each receive buffer element (handle).

OUTPUT PARAMETERS
-----------------

* ``count`` : Number of received elements (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns the number of entries received. (We count entries, each of type
datatype, not bytes.) The datatype argument should match the argument
provided by the receive call that set the status variable. (As explained
in Section 3.12.5 in the MPI-1 Standard, "Use of General Datatypes in
Communication," :ref:`MPI_Get_count` may, in certain situations, return the
value MPI_UNDEFINED.)

The datatype argument is passed to :ref:`MPI_Get_count` to improve performance.
A message might be received without counting the number of elements it
contains, and the count value is often not needed. Also, this allows the
same function to be used after a call to :ref:`MPI_Probe`.

NOTES
-----

If the size of the datatype is zero, this routine will return a count of
zero. If the amount of data in status is not an exact multiple of the
size of datatype (so that count would not be integral), a count of
MPI_UNDEFINED is returned instead.

ERRORS
------

.. include:: ./ERRORS.rst

If the value to be returned is larger than can fit into the count
parameter, an MPI_ERR_TRUNCATE error is raised.

.. seealso:: :ref:`MPI_Get_elements`
