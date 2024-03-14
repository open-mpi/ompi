.. _mpi_is_thread_main:


MPI_Is_thread_main
==================

.. include_body

:ref:`MPI_Is_thread_main` |mdash| Determines if thread called :ref:`MPI_Init`


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Is_thread_main(int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_IS_THREAD_MAIN(FLAG, IERROR)
   	LOGICAL	FLAG
   	INTEGER	IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Is_thread_main(flag, ierror)
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


OUTPUT PARAMETERS
-----------------
* ``flag``: True if calling thread is main thread (boolean).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Is_thread_main` is called by a thread to find out whether the caller
is the main thread (that is, the thread that called :ref:`MPI_Init` or
MPI_Init_thread).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Init_thread`
