.. _mpi_comm_idup:


MPI_Comm_idup
=============

.. include_body

:ref:`MPI_Comm_idup` |mdash| Start the nonblocking duplication of an existing
communicator with all its cached information.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_idup(MPI_Comm comm, MPI_Comm *newcomm, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_IDUP(COMM, NEWCOMM, REQUEST, IERROR)
   	INTEGER	COMM, NEWCOMM, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_idup(comm, newcomm, request, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``newcomm``: Copy of comm (handle).
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_idup` starts the nonblocking duplication of an existing
communicator comm with associated key values. For each key value, the
respective copy callback function determines the attribute value
associated with this key in the new communicator; one particular action
that a copy callback may take is to delete the attribute from the new
communicator. Returns in newcomm a new communicator with the same group,
any copied cached information, but a new context (see the
"Functionality" subsection of the "Caching" section in the "Groups,
Contexts, and Communicators" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_).  The communicator returned in
*newcomm* will not be available until the request is complete.

The completion of a communicator duplication request can be determined
by calling any of :ref:`MPI_Wait`, :ref:`MPI_Waitany`, :ref:`MPI_Test`, or :ref:`MPI_Testany` with
the request returned by this function.


NOTES
-----

This operation is used to provide a parallel library call with a
duplicate communication space that has the same properties as the
original communicator. This includes any attributes (see below) and
topologies (see the "Process Topologies" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_).
This call is valid even if there are pending point-to-point
communications involving the communicator comm. A typical call might
involve an :ref:`MPI_Comm_idup` at the beginning of the parallel call, and an
:ref:`MPI_Comm_free` of that duplicated communicator at the end of the call.
Other models of communicator management are also possible.

This call applies to both intra- and intercommunicators.

Note that it is not defined by the MPI standard what happens if the
attribute copy callback invokes other MPI functions. In Open MPI, it is
not valid for attribute copy callbacks (or any of their children) to add
or delete attributes on the same object on which the attribute copy
callback is being invoked.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_dup`
   * :ref:`MPI_Comm_dup_with_info`
