.. _mpi_comm_create_from_group:

MPI_Comm_create_from_group
==========================

.. include_body

:ref:`MPI_Comm_create_from_group` |mdash| Creates a new communicator from a group and
stringtag

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_create_from_group.rst

INPUT PARAMETERS
----------------

* ``group`` : Group (handler)
* ``stringtag`` : Unique identifier for this operation (string)
* ``info`` : info object (handler)
* ``errhandler`` : error handler to be attached to the new
   intra-communicator (handle)

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : New communicator (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_create_from_group` is similar to :ref:`MPI_Comm_create_group`, except
that the set of MPI processes involved in the creation of the new
intra-communicator is specified by a group argument, rather than the
group associated with a pre-existing communicator. If a non-empty group
is specified, then all MPI processes in that group must call the
function and each of these MPI processes must provide the same
arguments, including a group that contains the same members with the
same ordering, and identical stringtag value. In the event that
MPI_GROUP_EMPTY is supplied as the group argument, then the call is a
local operation and MPI_COMM_NULL is returned as newcomm. The stringtag
argument is analogous to the tag used for :ref:`MPI_Comm_create_group`. If
multiple threads at a given MPI process perform concurrent
:ref:`MPI_Comm_create_from_group` operations, the user must distinguish these
operations by providing different stringtag arguments. The stringtag
shall not exceed MPI_MAX_STRINGTAG_LEN characters in length. For C, this
includes space for a null terminating character.

NOTES
-----

The errhandler argument specifies an error handler to be attached to the
new intracommunicator. The info argument provides hints and assertions,
possibly MPI implementation dependent, which indicate desired
characteristics and guide communicator creation. MPI_MAX_STRINGTAG_LEN
shall have a value of at least 63.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_group`
