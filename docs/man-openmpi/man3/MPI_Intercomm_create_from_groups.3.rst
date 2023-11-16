.. _mpi_intercomm_create_from_groups:

MPI_Intercomm_create_from_groups
================================

.. include_body

:ref:`MPI_Intercomm_create_from_groups` |mdash| Creates a new inter-communicator from
a local and remote group and stringtag

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Intercomm_create_from_groups(MPI_Group local_group, int local_leader, MPI_Group remote_group, int remote_leader, const char *stringtag, MPI_Info info, MPI_Errhandler errhandler, MPI_Comm *newintercomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_INTERCOMM_CREATE_FROM_GROUPS(LOCAL_GROUP, LOCAL_LEADER, REMOTE_GROUP, REMOTE_LEADER, STRINGTAG, INFO, ERRHANDLER, NEWINTERCOMM, IERROR)
       INTEGER LOCAL_GROUP, LOCAL_LEADER, REMOTE_GROUP, REMOTE_LEADER, INFO, ERRHANDLER, NEWINTERCOMM, IERROR
       CHARACTER*(*) STRINGTAG

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Intercomm_create_from_groups(local_group, local_leader, remote_group, remote_leader, stringtag, info, errhandler, newintercomm, ierror)
       TYPE(MPI_Group), INTENT(IN) :: local_group, remote_group
       INTEGER, INTENT(IN) :: local_leader, remote_leader
       CHARACTER(LEN=*), INTENT(IN) :: stringtag
       TYPE(MPI_Info), INTENT(IN) :: info
       TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
       TYPE(MPI_Comm), INTENT(OUT) :: newintercomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``local_group`` : Local group (handler)
* ``local_leader`` : rank of local group leader in local_group (integer)
* ``remote_group`` : Remote group (handler)
* ``remote_leader`` : rank of remote leader in remote_group, significant
   only at local_leader (integer)
* ``stringtag`` : Unique identifier for this operation (string)
* ``info`` : info object (handler)
* ``errhandler`` : error handler to be attached to the new
   inter-communicator (handle)

OUTPUT PARAMETERS
-----------------

* ``newintercomm`` : New inter-communicator (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Intercomm_create_from_groups` creates an inter-communicator. Unlike
:ref:`MPI_Intercomm_create`, this function uses as input previously defined,
disjoint local and remote groups. The calling MPI process must be a
member of the local group. The call is collective over the union of the
local and remote groups. All involved MPI processes shall provide an
identical value for the stringtag argument. Within each group, all MPI
processes shall provide identical local_group, local_leader arguments.
Wildcards are not permitted for the remote_leader or local_leader
arguments. The stringtag argument serves the same purpose as the
stringtag used in the :ref:`MPI_Comm_create_from_group` function; it
differentiates concurrent calls in a multithreaded environment. The
stringtag shall not exceed MPI_MAX_STRINGTAG_LEN characters in length.
For C, this includes space for a null terminating character. In the
event that MPI_GROUP_EMPTY is supplied as the local_group or
remote_group1 or both, then the call is a local operation and
MPI_COMM_NULL is returned as the newintercomm.

NOTES
-----

The errhandler argument specifies an error handler to be attached to the
new inter-communicator. The info argument provides hints and assertions,
possibly MPI implementation dependent, which indicate desired
characteristics and guide communicator creation. MPI_MAX_STRINGTAG_LEN
shall have a value of at least 63.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Comm_create_from_group`
