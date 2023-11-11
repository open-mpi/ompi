.. _mpi_session_set_errhandler:


MPI_Session_set_errhandler
==========================

.. include_body

:ref:`MPI_Session_set_errhandler` |mdash| Attaches a new error handler to a
session.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_set_errhandler(MPI_Session session,
   	MPI_Errhandler errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SESSION_SET_ERRHANDLER(SESSION, ERRHANDLER, IERROR)
   	INTEGER	SESSION, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Session_set_errhandler(session, errhandler, ierror)
   	TYPE(MPI_Session), INTENT(IN) :: session
   	TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``session``: Session (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for session (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_set_errhandler` attaches a new error handler to a session.
The error handler must be either a predefined error handler or an error
handler created by a call to :ref:`MPI_Session_create_errhandler`.


ERRORS
------

.. include:: ./ERRORS.rst
