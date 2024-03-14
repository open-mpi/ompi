.. _mpi_get_elements:

MPI_Get_elements
================

.. include_body

:ref:`MPI_Get_elements`, :ref:`MPI_Get_elements_x` - Returns the number of basic
elements in a data type.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Get_elements(const MPI_Status *status, MPI_Datatype datatype,
       int *count)

   int MPI_Get_elements_x(const MPI_Status *status, MPI_Datatype datatype,
       MPI_Count *count)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_ELEMENTS(STATUS, DATATYPE, COUNT, IERROR)
       INTEGER STATUS(MPI_STATUS_SIZE), DATATYPE, COUNT, IERROR

   MPI_GET_ELEMENTS_X(STATUS, DATATYPE, COUNT, IERROR)
       INTEGER STATUS(MPI_STATUS_SIZE), DATATYPE
           INTEGER(KIND=MPI_COUNT_KIND) COUNT
           INTEGER IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Get_elements(status, datatype, count, ierror)
       TYPE(MPI_Status), INTENT(IN) :: status
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       INTEGER, INTENT(OUT) :: count
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Get_elements_x(status, datatype, count, ierror)
       TYPE(MPI_Status), INTENT(IN) :: status
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       INTEGER(KIND = MPI_COUNT_KIND), INTENT(OUT) :: count
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``status`` : Return status of receive operation (status).
* ``datatype`` : Datatype used by receive operation (handle).

OUTPUT PARAMETERS
-----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Get_elements` and :ref:`MPI_Get_elements_x` behave different from
:ref:`MPI_Get_count`, which returns the number of "top-level entries" received,
i.e., the number of "copies" of type datatype. :ref:`MPI_Get_count` may return
any integer value k, where 0 =< k =< count. If :ref:`MPI_Get_count` returns k,
then the number of basic elements received (and the value returned by
:ref:`MPI_Get_elements` and MPI_Get_elements_x) is n k, where n is the number
of basic elements in the type map of datatype. If the number of basic
elements received is not a multiple of n, that is, if the receive
operation has not received an integral number of datatype "copies," then
:ref:`MPI_Get_count` returns the value MPI_UNDEFINED. For both functions, if
the count parameter cannot express the value to be returned (e.g., if
the parameter is too small to hold the output value), it is set to
MPI_UNDEFINED.

Example: Usage of :ref:`MPI_Get_count` and :ref:`MPI_Get_elements`:

.. code-block:: fortran

   call MPI_TYPE_CONTIGUOUS(2, MPI_REAL, Type2, ierr)
   call MPI_TYPE_COMMIT(Type2, ierr)
   call MPI_COMM_RANK(comm, rank, ierr)

   IF ( rank == 0 ) THEN
      CALL MPI_SEND(a, 2, MPI_REAL, 1, 0, comm, ierr)
      CALL MPI_SEND(a, 3, MPI_REAL, 1, 0, comm, ierr)
   ELSE
      CALL MPI_RECV(a, 2, Type2, 0, 0, comm, stat, ierr)
      CALL MPI_GET_COUNT(stat, Type2, i, ierr) ! returns i=1
      CALL MPI_GET_ELEMENTS(stat, Type2, i, ierr) ! returns i=2
      CALL MPI_RECV(a, 2, Type2, 0, 0, comm, stat, ierr)
      CALL MPI_GET_COUNT(stat, Type2, i, ierr) ! returns i=MPI_UNDEFINED
      CALL MPI_GET_ELEMENTS(stat, Type2, i, ierr)  ! returns i=3
   END IF

The function :ref:`MPI_Get_elements` can also be used after a probe to find the
number of elements in the probed message. Note that the two functions
:ref:`MPI_Get_count` and :ref:`MPI_Get_elements` return the same values when they are
used with primitive data types.


ERRORS
------

.. include:: ./ERRORS.rst


.. seealso:: :ref:`MPI_Get_count`
