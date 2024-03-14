.. _mpi_pready_range:


MPI_Pready_range
================

.. include_body

:ref:`MPI_Pready_range` |mdash| Indicates that a given range os send-side
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
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready`
   * :ref:`MPI_Pready_list`
   * :ref:`MPI_Parrived`
