.. _mpi_init:


MPI_Init
========

.. include_body

:ref:`MPI_Init` |mdash| Initializes the MPI execution environment


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Init(int *argc, char ***argv)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INIT(IERROR)
   	INTEGER	IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Init(ierror)
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``argc``: C only: Pointer to the number of arguments.
* ``argv``: C only: Argument vector.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine, or :ref:`MPI_Init_thread`, must be called before most other MPI
routines are called. There are a small number of errors, such as
:ref:`MPI_Initialized` and :ref:`MPI_Finalized`. MPI can be initialized at most once;
subsequent calls to :ref:`MPI_Init` or :ref:`MPI_Init_thread` are erroneous.

All MPI programs must contain a call to :ref:`MPI_Init` or :ref:`MPI_Init_thread`.
Open MPI accepts the C *argc* and *argv* arguments to main, but neither
modifies, interprets, nor distributes them:

.. code-block:: c

   /* declare variables */
   MPI_Init(&argc, &argv);
   /* parse arguments */
   /* main program */
   MPI_Finalize();


NOTES
-----

The Fortran version does not have provisions for *argc* and *argv* and
takes only IERROR.

The MPI Standard does not say what a program can do before an :ref:`MPI_Init`
or after an :ref:`MPI_Finalize`. In the Open MPI implementation, it should do
as little as possible. In particular, avoid anything that changes the
external state of the program, such as opening files, reading standard
input, or writing to standard output.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init_thread`
   * :ref:`MPI_Initialized`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
