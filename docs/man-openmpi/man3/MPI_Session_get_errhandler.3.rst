.. _mpi_session_get_errhandler:


MPI_Session_get_errhandler
==========================

.. include_body

:ref:`MPI_Session_get_errhandler` |mdash| Retrieves error handler associated with a
session.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_get_errhandler(MPI_Session session,
   	MPI_Errhandler *errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SESSION_GET_ERRHANDLER(SESSION, ERRHANDLER, IERROR)
   	INTEGER	SESSION, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Session_get_errhandler(session, errhandler, ierror)
   	TYPE(MPI_Session), INTENT(IN) :: session
   	TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``session``: Session (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for session (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_errhandler` retrieves the error handler currently associated
with a session. 


ERRORS
------

.. include:: ./ERRORS.rst
