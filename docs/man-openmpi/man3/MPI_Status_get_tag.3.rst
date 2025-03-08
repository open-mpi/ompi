.. _mpi_status_get_tag:


MPI_Status_get_tag
========================

.. include_body

:ref:`MPI_Status_get_tag` |mdash| Retrieves the MPI_TAG field from *status*.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Status_get_tag(MPI_Status *status, int *tag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_STATUS_GET_TAG(STATUS, TAG, IERROR)
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR
   	INTEGER TAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Status_get_tag(status, tag, ierror)
        TYPE(MPI_Status), INTENT(IN) :: status
        INTEGER, INTENT(OUT) :: tag
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``status``: Status from which to retrieve the tag (status).

OUTPUT PARAMETER
----------------
* ``tag``: tag set in the MPI_TAG field (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns in tag the MPI_TAG field from the status object.


ERRORS
------

.. include:: ./ERRORS.rst
