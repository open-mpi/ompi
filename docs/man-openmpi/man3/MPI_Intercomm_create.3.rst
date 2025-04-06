.. _mpi_intercomm_create:


MPI_Intercomm_create
====================

.. include_body

:ref:`MPI_Intercomm_create` |mdash| Creates an intercommunicator from two
intracommunicators.

.. The following file was automatically generated
.. include:: ./bindings/mpi_intercomm_create.rst

INPUT PARAMETERS
----------------
* ``local_comm``: The communicator containing the process that initiates the inter-communication (handle).
* ``local_leader``: Rank of local group leader in local_comm (integer).
* ``peer_comm``: "Peer" communicator; significant only at the local_leader (handle).
* ``remote_leader``: Rank of remote group leader in peer_comm; significant only at the local_leader (integer).
* ``tag``: Message tag used to identify new intercommunicator (integer).

OUTPUT PARAMETERS
-----------------
* ``newintercomm``: Created intercommunicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This call creates an intercommunicator. It is collective over the union
of the local and remote groups. Processes should provide identical
local_comm and local_leader arguments within each group. Wildcards are
not permitted for remote_leader, local_leader, and tag.

This call uses point-to-point communication with communicator peer_comm,
and with tag tag between the leaders. Thus, care must be taken that
there be no pending communication on peer_comm that could interfere with
this communication.

If multiple MPI_Intercomm_creates are being made, they should use
different tags (more precisely, they should ensure that the local and
remote leaders are using different tags for each MPI_intercomm_create).


NOTES
-----

We recommend using a dedicated peer communicator, such as a duplicate of
MPI_COMM_WORLD, to avoid trouble with peer communicators.

The MPI 1.1 Standard contains two mutually exclusive comments on the
input intracommunicators. One says that their respective groups must be
disjoint; the other that the leaders can be the same process. After some
discussion by the MPI Forum, it has been decided that the groups must be
disjoint. Note that the **reason** given for this in the standard is
**not** the reason for this choice; rather, the **other** operations on
intercommunicators (like :ref:`MPI_Intercomm_merge` ) do not make sense if
the groups are not disjoint.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Intercomm_merge`
   * :ref:`MPI_Comm_free`
   * :ref:`MPI_Comm_remote_group`
   * :ref:`MPI_Comm_remote_size`
