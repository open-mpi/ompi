.. _mpi_comm_set_errhandler:


MPI_Comm_set_errhandler
=======================

.. include_body

:ref:`MPI_Comm_set_errhandler` |mdash| Attaches a new error handler to a
communicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_set_errhandler(MPI_Comm comm,
   	MPI_Errhandler errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_SET_ERRHANDLER(COMM, ERRHANDLER, IERROR)
   	INTEGER	COMM, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_set_errhandler(comm, errhandler, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for communicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_set_errhandler` attaches a new error handler to a communicator.
The error handler must be either a predefined error handler or an error
handler created by a call to :ref:`MPI_Comm_create_errhandler`. This call is
identical to :ref:`MPI_Errhandler_set`, the use of which is deprecated.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_errhandler`
   * :ref:`MPI_Comm_get_errhandler`
