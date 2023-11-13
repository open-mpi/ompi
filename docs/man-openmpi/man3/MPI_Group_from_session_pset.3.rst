.. _mpi_group_from_session_pset:

MPI_Group_from_session_pset
===========================

.. include_body

:ref:`MPI_Group_from_session_pset` |mdash| Creates a group using a provided session
handle and process set.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_from_session_pset(MPI_Session session, const char *pset_name, MPI_Group *newgroup)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GROUP_FROM_SESSION_PSET(SESSION, PSET_NAME, NEWGROUP, IERROR)
       INTEGER SESSION,  NEWGROUP, IERROR
       CHARACTER*(*) PSET_NAME

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Group_from_session_pset(session, pset_name, newgroup, ierror)
       TYPE(MPI_Session), INTENT(IN) :: session
       CHARACTER(LEN=*), INTENT(IN) :: pset_name
       TYPE(MPI_Group), INTENT(OUT) :: newgroup
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``session`` : Session (handle).
* ``pset_name`` : name of process set to use to create the new group
   (string)

OUTPUT PARAMETERS
-----------------

* ``newgroup`` : New group derived from supplied session and process set
   (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Group_from_session_pset` creates a group newgroup using
the provided session handle and process set. The process set name must
be one returned from an invocation of :ref:`MPI_Session_get_nth_pset` using the
supplied session handle. If the pset_name does not exist, MPI_GROUP_NULL
will be returned in the newgroup argument.

NOTE
----

As with other group constructors, :ref:`MPI_Group_from_session_pset` is a local
function.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
