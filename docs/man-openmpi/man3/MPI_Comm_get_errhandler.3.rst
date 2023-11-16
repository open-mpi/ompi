.. _mpi_comm_get_errhandler:


MPI_Comm_get_errhandler
=======================

.. include_body

:ref:`MPI_Comm_get_errhandler` |mdash| Retrieves error handler associated with a
communicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_get_errhandler(MPI_Comm comm,
   	MPI_Errhandler *errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_GET_ERRHANDLER(COMM, ERRHANDLER, IERROR)
   	INTEGER	COMM, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_get_errhandler(comm, errhandler, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for communicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_get_errhandler` retrieves the error handler currently associated
with a communicator. This call is identical to :ref:`MPI_Errhandler_get`, the
use of which is deprecated.


Returns in errhandler (a handle to) the error handler that is currently
associated with communicator ``comm``.

**Example:** A library function may register at its entry point the
current error handler for a communicator, set its own private error
handler for this communicator, and restore before exiting the previous
error handler.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_errhandler`
   * :ref:`MPI_Comm_set_errhandler`
