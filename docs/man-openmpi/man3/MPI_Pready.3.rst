.. _mpi_pready:


MPI_Pready
==========

.. include_body

:ref:`MPI_Pready` |mdash| Indicates that a given send-side partition is ready to
be transferred.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Pready(int partition, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PREADY(PARTITION, REQUEST, IERROR)
   	INTEGER	PARTITION, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Pready(partition, request, ierror)
   	INTEGER, INTENT(IN) :: partition
   	TYPE(MPI_Request), INTENT(IN) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``partition``: The number of the partition to mark ready for transfer (integer).
* ``request``: Communication request (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready_list`
   * :ref:`MPI_Pready_range`
   * :ref:`MPI_Parrived`
