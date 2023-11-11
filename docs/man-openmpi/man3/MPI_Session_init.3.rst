.. _mpi_session_init:

MPI_Session_init
================

.. include_body

:ref:`MPI_Session_init` |mdash| Creates a new session handle

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_init(MPI_Info info, MPI_Errhandler errhandler, MPI_Session *session)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_INIT(INFO, ERRHANDLER, SESSION, IERROR)
       INTEGER INFO, ERRHANDLER, SESSION, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Session_init(info, errhandler, session, ierror)
       TYPE(MPI_Info), INTENT(IN) :: info
       TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
       TYPE(MPI_Session), INTENT(OUT) :: session
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``info`` : info object (handle)
* ``errhandler`` : error handler to be attached to the returned session
   (handle)

OUTPUT PARAMETERS
-----------------

* ``session`` : New session (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_init` is used to instantiate an MPI Session. The returned
session handle can be used to query the runtime system about
characteristics of the job within which the process is running, as well
as other system resources. An application can make multiple calls to
:ref:`MPI_Session_init` and the related :ref:`MPI_Session_finalize` routine.

NOTES
-----

The info argument is used to request MPI functionality requirements and
possible MPI implementation specific capabilities.

The errhandler argument specifies an error handler to invoke in the
event that the Session instantiation call encounters an error.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_get_num_psets` MPI_Session_group_from_pset
