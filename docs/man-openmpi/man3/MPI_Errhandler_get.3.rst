.. _mpi_errhandler_get:


MPI_Errhandler_get
==================

.. include_body

:ref:`MPI_Errhandler_get` |mdash| Gets the error handler for a communicator -- |deprecated_favor| :ref:`MPI_Comm_get_errhandler`.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_ERRHANDLER_GET(COMM, ERRHANDLER, IERROR)
   	INTEGER	COMM, ERRHANDLER, IERROR


INPUT PARAMETER
---------------
* ``comm``: Communicator to get the error handler from (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: MPI error handler currently associated with communicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Comm_get_errhandler` instead.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_errhandler`
   * :ref:`MPI_Comm_get_errhandler`
   * :ref:`MPI_Comm_set_errhandler`
