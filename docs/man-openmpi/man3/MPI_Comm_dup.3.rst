.. _mpi_comm_dup:

MPI_Comm_dup
============

.. include_body

:ref:`MPI_Comm_dup` |mdash| Duplicates an existing communicator with all its cached
information.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_dup.rst

INPUT PARAMETER
---------------

* ``comm`` : Communicator (handle).

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : Copy of comm (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_dup` duplicates the existing communicator comm with associated
key values. For each key value, the respective copy callback function
determines the attribute value associated with this key in the new
communicator; one particular action that a copy callback may take is to
delete the attribute from the new communicator. Returns in newcomm a new
communicator with the same group, any copied cached information, but a
new context (see the "Functionality" subsection of the "Caching"
section in the "Groups, Contexts, and Communicators" chapter in the
`MPI Standard <https://www.mpi-forum.org/docs/>`_).


NOTES
-----

This operation is used to provide a parallel library call with a
duplicate communication space that has the same properties as the
original communicator. This includes any attributes (see below) and
topologies (see the "Process Topologies" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_).
This call is valid even if there are pending point-to-point
communications involving the communicator comm. A typical call might
involve an :ref:`MPI_Comm_dup` at the beginning of the parallel call, and an
:ref:`MPI_Comm_free` of that duplicated communicator at the end of the call.
Other models of communicator management are also possible. This call
applies to both intra- and intercommunicators. Note that it is not
defined by the MPI standard what happens if the attribute copy callback
invokes other MPI functions. In Open MPI, it is not valid for attribute
copy callbacks (or any of their children) to add or delete attributes on
the same object on which the attribute copy callback is being invoked.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Comm_dup_with_info`
