.. _mpi_session_get_info:

MPI_Session_get_info
====================

.. include_body

:ref:`MPI_Session_get_info` |mdash| Returns an info object containing the hints of an
MPI Session

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_get_info(MPI_Session session, MPI_Info *info_used)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_GET_INFO(SESSION, INFO_USED)
       INTEGER SESSION, INFO_USED

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Session_get_info(session, info_used)
       TYPE(MPI_Session), INTENT(IN) :: session
       TYPE(MPI_Info), INTENT(OUT) :: info_used

INPUT PARAMETERS
----------------

* ``session`` : session (handle)

OUTPUT PARAMETERS
-----------------

* ``info_used`` : info object (handle)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_info` returns a new info object containing the hints of
the MPI Session associated with session. The current setting of all
hints related to this MPI Session is returned in info_used. An MPI
implementation is required to return all hints that are supported by the
implementation and have default values specified; any user-supplied
hints that were not ignored by the implementation; and any additional
hints that were set by the implementation. If no such hints exist, a
handle to a newly created info object is returned that contains no
key/value pair.

NOTES
-----

The user is responsible for freeing info_used via :ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
