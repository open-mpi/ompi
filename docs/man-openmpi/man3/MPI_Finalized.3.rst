.. _mpi_finalized:

MPI_Finalized
=============

.. include_body

:ref:`MPI_Finalized` |mdash| Checks whether MPI has been finalized

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Finalized(int *flag)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_FINALIZED(FLAG, IERROR)
       LOGICAL FLAG
       INTEGER IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Finalized(flag, ierror)
       LOGICAL, INTENT(OUT) :: flag
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

OUTPUT PARAMETER
----------------

* ``flag`` : True if MPI was finalized, and false otherwise (logical).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine may be used to determine whether MPI has been finalized. It
is one of a small number of routines that may be called before MPI is
initialized and after MPI has been finalized (:ref:`MPI_Initialized` is
another).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Init`
