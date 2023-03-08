.. _mpix_comm_revoke:

MPIX_Comm_revoke
================
.. include_body

:ref:`MPIX_Comm_revoke` - Revoke a communicator, causing errors to be
raised, at all ranks, for non-local operations on the communicator.

This is part of the User Level Fault Mitigation :ref:`ULFM extension <ulfm-label>`.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Comm_revoke(MPI_Comm comm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   USE MPI_EXT
   ! or the older form: INCLUDE 'mpif.h'

   MPIX_COMM_REVOKE(COMM, IERROR)
        INTEGER COMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   USE mpi_ext_f08

   MPIX_Comm_revoke(comm, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function marks the communicator *comm* as revoked at all MPI processes
in the groups (local and remote) associated with the communicator *comm*.
This function is not collective and therefore does not have a matching call
on remote MPI processes.

The documentation for :ref:`MPIX_Comm_is_revoked` details the conditions
for when a communicator becomes revoked locally, and what semantics apply
on a revoked communicator. In summary, when a communicator is revoked,
non-local operation raise an exception of class MPIX_ERR_REVOKED,
except for select fault-tolerant operations.

PROPAGATION OF THE REVOKED STATE AND ORDERING
---------------------------------------------

The implementation propagates the revoked state in a fault-tolerant manner;
thus, the communicator becomes revoked at all non-failed MPI processes
belonging to *comm* despite failed processes.

There is no particular ordering between the revocation call at another
process and the completion of operations at a local process, for example,
a receive operation can raise an error of class MPIX_ERR_REVOKED, even if
the send operation procedure is called before the revoke procedure at the
sender.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPIX_Comm_is_revoked`
   * :ref:`MPIX_Comm_agree`
   * :ref:`MPIX_Comm_shrink`
