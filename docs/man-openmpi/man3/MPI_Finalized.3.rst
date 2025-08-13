.. _mpi_finalized:

MPI_Finalized
=============

.. include_body

:ref:`MPI_Finalized` |mdash| Checks whether the MPI world model has been finalized

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

* ``flag`` : True if the MPI world model was finalized, and false
  otherwise (logical).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine may be used to determine whether the MPI world model has
been finalized.  A different routine |mdash| :ref:`MPI_Initialized`
|mdash| is used to indicate whether the MPI world model has been
initialized.

See `MPI-5.0:11.4.1 <https://www.mpi-forum.org/>`_ for a list of MPI
functionality that is available (e.g., even when the MPI
world model has not yet initialized or has already been finalized).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Init_thread`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
   * :ref:`MPI_Session_init`
   * :ref:`MPI_Session_finalize`
