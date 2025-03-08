.. _mpi_status_set_error:


MPI_Status_set_error
========================

.. include_body

:ref:`MPI_Status_set_error` |mdash| Sets the MPI_ERROR field on *status*.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Status_set_error(MPI_Status *status, int error)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_STATUS_SET_ERROR(STATUS, ERROR, IERROR)
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR
   	INTEGER ERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Status_set_error(status, error, ierror)
   	TYPE(MPI_Status), INTENT(INOUT) :: status
   	INTEGER, INTENT(IN) :: error
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``status``: Status with which to associate error (status).

INPUT PARAMETER
---------------
* ``error``: error to set in the MPI_ERROR field (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Set the MPI_ERROR field in the status object to the provided error argument.


ERRORS
------

.. include:: ./ERRORS.rst
