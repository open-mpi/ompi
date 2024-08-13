.. _mpi_pready_list:


MPI_Pready_list
===============

.. include_body

:ref:`MPI_Pready_list` |mdash| Indicates that a list given send-side partitions
are ready to be transferred.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Pready_list(int length, int *partitions, MPI_Request request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PREADY_LIST(LENGTH, PARTITIONS, REQUEST, IERROR)
   	INTEGER	LENGTH, PARTITIONS(*), REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Pready_list(length, partitions, request, ierror)
           INTEGER, INTENT(IN) :: length
   	INTEGER, INTENT(IN) :: partitions
   	TYPE(MPI_Request), INTENT(IN) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``length``: The length of the given partition array (integer).
* ``partitions``: An array of numbers of partitions to mark ready for transfer (integer).
* ``request``: Communication request (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready`
   * :ref:`MPI_Pready_range`
   * :ref:`MPI_Parrived`
