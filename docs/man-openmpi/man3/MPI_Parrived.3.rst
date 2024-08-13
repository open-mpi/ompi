.. _mpi_parrived:


MPI_Parrived
============

.. include_body

:ref:`MPI_Parrived` |mdash| Tests for completion of a specified receive-side
partition.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Parrived(MPI_Request request, int partition, int *flag)


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
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready_list`
   * :ref:`MPI_Pready_range`
   * :ref:`MPI_Parrived`
