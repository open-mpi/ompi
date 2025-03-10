.. _mpi_status_set_tag:


MPI_Status_set_tag
========================

.. include_body

:ref:`MPI_Status_set_tag` |mdash| Sets the ``MPI_TAG`` field on ``status``.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Status_set_tag(MPI_Status *status, int tag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_STATUS_SET_TAG(STATUS, TAG, IERROR)
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR
   	INTEGER TAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Status_set_tag(status, tag, ierror)
   	TYPE(MPI_Status), INTENT(INOUT) :: status
   	INTEGER, INTENT(IN) :: tag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``status``: Status with which to associate tag (status).

INPUT PARAMETER
---------------
* ``tag``: tag to set in the ``MPI_TAG`` field (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Set the ``MPI_TAG`` field in the ``status`` object to the provided tag
argument.

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
   * :ref:`MPI_Status_get_error`
   * :ref:`MPI_Status_get_source`
   * :ref:`MPI_Status_get_tag`
   * :ref:`MPI_Status_set_error`
   * :ref:`MPI_Status_set_source`
