.. _mpi_session_get_pset_info:

MPI_Session_get_pset_info
=========================

.. include_body

:ref:`MPI_Session_get_pset_info` |mdash| Returns an info object containing properties
of a specific process set

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_get_pset_info(MPI_Session session, const char *pset_name, MPI_Info *info)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_GET_PSET_INFO(SESSION, PSET_NAME, INFO, IERROR)
       INTEGER SESSION, INFO, IERROR
       CHARACTER*(*) PSET_NAME

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Session_get_pset_info(session, pset_name, info, ierror)
       TYPE(MPI_Session), INTENT(IN) :: session
       CHARACTER(LEN=*), INTENT(IN) :: pset_name
       TYPE(MPI_Info), INTENT(OUT) :: info
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``session`` : session (handle)
* ``pset_name`` : name of process set (string)

OUTPUT PARAMETERS
-----------------

* ``info`` : info object (handle)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_pset_info` is used to query properties of a specific
process set. The returned info object can be queried with existing MPI
info object query functions. One key/value pair must be defined,
"mpi_size". The value of the "mpi_size" key specifies the number of MPI
processes in the process set.

NOTES
-----

The user is responsible for freeing the returned info object via
:ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
