.. _mpi_status_get_error:


MPI_Status_get_error
========================

.. include_body

:ref:`MPI_Status_get_error` |mdash| Retrieves the ``MPI_ERROR`` field from ``status``.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Status_get_error(MPI_Status *status, int *error)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_STATUS_GET_ERROR(STATUS, ERROR, IERROR)
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR
   	INTEGER ERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Status_get_error(status, error, ierror)
        TYPE(MPI_Status), INTENT(IN) :: status
        INTEGER, INTENT(OUT) :: error
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
---------------
* ``status``: Status from which to retrieve the error (status).

OUTPUT PARAMETER
----------------
* ``error``: error set in the ``MPI_ERROR`` field (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns in error the ``MPI_ERROR`` field from the ``status`` object.

While the ``status`` object members ``MPI_SOURCE``, ``MPI_TAG``, and
``MPI_ERROR`` are directly accessible in C and Fortran, for
convenience in other contexts (e.g., when using alternate MPI bindings
in languages that do not directly translate the ``status`` object),
users can also access these values via procedure calls such as this
one.


ERRORS
------

.. include:: ./ERRORS.rst


.. seealso::
   * :ref:`MPI_Status_get_source`
   * :ref:`MPI_Status_get_tag`
   * :ref:`MPI_Status_set_error`
   * :ref:`MPI_Status_set_source`
   * :ref:`MPI_Status_set_tag`
