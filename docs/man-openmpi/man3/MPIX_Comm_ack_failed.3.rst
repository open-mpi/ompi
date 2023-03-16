.. _mpix_comm_ack_failed:

MPIX_Comm_ack_failed
====================
.. include_body

:ref:`MPIX_Comm_get_failed` - acknowledge failed processes in a communicator.

This is part of the User Level Fault Mitigation :ref:`ULFM extension <ulfm-label>`.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Comm_ack_failed(MPI_Comm comm, int num_to_ack, int *num_acked)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   USE MPI_EXT
   ! or the older form: INCLUDE 'mpif.h'

   MPIX_COMM_ACK_FAILED(COMM, NUM_TO_ACK, NUM_ACKED, IERROR)
        INTEGER COMM, NUM_TO_ACK, NUM_ACKED, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   USE mpi_ext_f08

   MPIX_Comm_ack_failed(comm, num_to_ack, num_acked, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(IN) :: num_to_ack
        INTEGER, INTENT(OUT) :: num_acked
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).
* ``num_to_ack``: maximum number of process failures to acknowledge in *comm* (integer)

OUTPUT PARAMETERS
-----------------
* ``num_acked``: number of acknowledged failures in *comm* (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

his local operation gives the users a way to **acknowledge**
locally notified failures on *comm*. The operation acknowledges the first
*num_to_ack* process failures on *comm*, that is, it acknowledges the
failure of members with a rank lower than *num_to_ack* in the group that
would be produced by a concurrent call to :ref:`MPIX_Comm_get_failed` on
the same *comm*.

The operation also sets the value of *num_acked* to the current number of
acknowledged process failures in *comm*, that is, a process failure has been
acknowledged on *comm* if and only if the rank of the process is lower than
*num_acked* in the group that would be produced by a subsequent call to
:ref:`MPIX_Comm_get_failed` on the same *comm*.

*num_acked* can be larger than *num_to_ack* when process failures have been
acknowledged in a prior call to :ref:`MPIX_Comm_ack_failed`.

EFFECT OF ACKNOWLEDGING FAILURES
--------------------------------

After an MPI process failure is acknowledged on *comm*, unmatched
MPI_ANY_SOURCE receive operations on the same *comm* that would have raised
an error of class MPIX_ERR_PROC_FAILED_PENDING proceed without further raising
errors due to this acknowledged failure.

Also, :ref:`MPIX_Comm_agree` on the same *comm* will not raise an error of
class MPI_ERR_PROC_FAILED due to this acknowledged failure.

USAGE PATTERNS
--------------

One may query, without side effect, for the number of currently aknowledged
process failures *comm* by supplying 0 in *num_to_ack*.

Conversely, one may unconditionally acknowledge all currently known process
failures in *comm* by supplying the size of the group of *comm* in *num_to_ack*.

Note that the number of acknowledged processes, as returned in *num_acked*,
can be smaller or larger than the value supplied in *num_to_ack*; It is
however never larger than the size of the group returned by a subsequent call
to :ref:`MPIX_Comm_get_failed`.

EFFECT ON COLLECTIVE OPERATIONS
-------------------------------

Calling :ref:`MPIX_Comm_ack_failed` on a communicator with failed MPI
processes has no effect on collective operations (except for :ref:`MPIX_Comm_agree`).
If a collective operation would raise an error due to the communicator
containing a failed process it will continue to raise an error even after
the failure has been acknowledged. In order to use collective operations
between MPI processes of a communicator that contains failed MPI processes,
users should create a new communicator (e.g., by calling :ref:`MPIX_Comm_shrink`).

WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR
------------------------------------------

When the communicator is an inter-communicator, the failures of members
in both the local and the remote groups of *comm* are acknowledged.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPIX_Comm_get_failed`
   * :ref:`MPIX_Comm_agree`
