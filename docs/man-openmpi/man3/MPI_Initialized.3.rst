.. _mpi_initialized:


MPI_Initialized
===============

.. include_body

:ref:`MPI_Initialized` - Checks whether MPI has been initialized


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Initialized(int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INITIALIZED(FLAG, IERROR)
   	LOGICAL	FLAG
   	INTEGER	IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Initialized(flag, ierror)
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


OUTPUT PARAMETERS
-----------------
* ``flag``: True if MPI has been initialized, and false otherwise (logical).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine may be used to determine whether MPI has been initialized.
It is one of a small number of routines that may be called before MPI is
initialized and after MPI has been finalized (:ref:`MPI_Finalized` is another).


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Init` :ref:`MPI_Init_thread` :ref:`MPI_Finalize` :ref:`MPI_Finalized`
