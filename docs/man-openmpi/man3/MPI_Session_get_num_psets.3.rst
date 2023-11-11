.. _mpi_session_get_num_psets:

MPI_Session_get_num_psets
=========================

.. include_body

:ref:`MPI_Session_get_num_psets` |mdash| Query runtime for number of available
process sets

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_get_num_psets(MPI_Session session, MPI_Info info, int *npset_names)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_GET_NUM_PSETS(SESSION, INFO, NPSET_NAMES, IERROR)
       INTEGER SESSION, INFO, SESSION, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Session_get_num_psets(session, info, npset_names, ierror)
       TYPE(MPI_Session), INTENT(IN) :: session
       TYPE(MPI_Info), INTENT(IN) :: info
       INTEGER, INTENT(OUT) :: npset_names
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``session`` : session (handle)
* ``info`` : info object (handle)

OUTPUT PARAMETERS
-----------------

* ``npset_names`` : number of available process sets (non-negtive integer)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_num_psets` is used to query the runtime for the number of
available process sets in which the calling MPI process is a member. An
MPI implementation is allowed to increase the number of available
process sets during the execution of an MPI application when new process
sets become available. However, MPI implementations are not allowed to
change the index of a particular process set name, or to change the name
of the process set at a particular index, or to delete a process set
name once it has been added.

NOTES
-----

When a process set becomes invalid, for example, when some processes
become unreachable due to failures in the communication system,
subsequent usage of the process set name may raise an error. For
example, creating an MPI_Group from such a process set might succeed
because it is a local operation, but creating an MPI_Comm from that
group and attempting collective communication may raise an error.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
