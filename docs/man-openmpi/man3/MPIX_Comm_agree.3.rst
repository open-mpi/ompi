.. _mpix_comm_agree:

MPIX_Comm_agree
===============
.. include_body

:ref:`MPIX_Comm_agree`, :ref:`MPIX_Comm_iagree` - Agree on a flag value
from all live processes and distributes the result back to all live
processes, even after process failures.

This is part of the User Level Fault Mitigation :ref:`ULFM extension <ulfm-label>`.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Comm_agree(MPI_Comm comm, int *flag)
   
   int MPIX_Comm_iagree(MPI_Comm comm, int *flag, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   USE MPI_EXT
   ! or the older form: INCLUDE 'mpif.h'

   MPIX_COMM_AGREE(COMM, FLAG, IERROR)
        INTEGER COMM, FLAG, IERROR

   MPIX_COMM_IAGREE(COMM, FLAG, REQUEST, IERROR)
        INTEGER COMM, FLAG, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   USE mpi_ext_f08

   MPIX_Comm_agree(comm, flag, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(INOUT) :: flag
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPIX_COMM_IAGREE(COMM, FLAG, REQUEST, IERROR)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(INOUT), ASYNCHRONOUS :: flag
        TYPE(MPI_Request), INTENT(OUT) :: request
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).
* ``flag``: Binary flags (integer).

OUTPUT PARAMETERS
-----------------
* ``flag``: Reduced binary flags (integer).
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This collective communication agrees on the integer value *flag* and
(implicitly) on the group of failed processes in *comm*.

On completion, all non-failed MPI processes have agreed to set the
output integer value of *flag* to the result of a *bitwise AND*
operation over the contributed input values of *flag*.

:ref:`MPIX_Comm_iagree` is the non-blocking variant of :ref:`MPIX_Comm_agree`.

PROCESS FAILURES
----------------

When an MPI process fails before contributing to the agree operation,
the *flag* is computed ignoring its contribution, and the operation
raises an error of class MPIX_ERR_PROC_FAILED.

When an error of class MPIX_ERR_PROC_FAILED is raised, it is consistently
raised at all MPI processes in the group(s) of *comm*.

After :ref:`MPIX_Comm_agree` raised an error of class MPIX_ERR_PROC_FAILED,
the group produced by a subsequent call to :ref:`MPIX_Comm_get_failed` on
*comm* contains every MPI process that didn't contribute to the
computation of *flag*.

WHEN THE COMMUNICATOR CONTAINS ACKNOWLEDGED FAILURES
----------------------------------------------------

If **all** MPI processes in the group of *comm* have acknowledged the failure
of an MPI process (using :ref:`MPIX_Comm_ack_failed`) prior to the call to
:ref:`MPIX_Comm_agree` (or :ref:`MPIX_Comm_iagree`), the MPIX_ERR_PROC_FAILED
error is not raised when the output value of *flag* ignores the
contribution of that failed process. Note that this is an uniform property:
if a non-contributing process is found to be not-acknowledged at any live
process in *comm*, all processes raise an error of class MPIX_ERR_PROC_FAILED.

**Example 1:** Using a combination of :ref:`MPIX_Comm_ack_failed` and
:ref:`MPIX_Comm_agree` users can propagate and synchronize the knowledge
of failures across all MPI processes in *comm*.

.. code-block:: c

    Comm_get_failed_consistent(MPI_Comm c, MPI_Group * g) {
        int rc; int T=1;
        int size; int num_acked;
        MPI_Group gf;
        int ranges[3] = {0, 0, 1};

        MPI_Comm_size(c, &size);

        do {
            /* this routine is not pure: calling MPI_Comm_ack_failed
             * affects the state of the communicator c */
            MPIX_Comm_ack_failed(c, size, &num_acked);
            /* we simply ignore the T value in this example */
            rc = MPIX_Comm_agree(c, &T);
        } while( rc != MPI_SUCCESS );
        /* after this loop, MPIX_Comm_agree has returned MPI_SUCCESS at
         * all processes, so all processes have Acknowledged the same set of
         * failures. Let's get that set of failures in the g group. */
        if( 0 == num_acked ) {
            *g = MPI_GROUP_EMPTY;
        }
        else {
            MPIX_Comm_get_failed(c, &gf);
            ranges[1] = num_acked - 1;
            MPI_Group_range_incl(gf, 1, ranges, g);
            MPI_Group_free(&gf);
        }
    }

WHEN THE COMMUNICATOR IS REVOKED
--------------------------------

This function never raises an error of class MPIX_ERR_REVOKED.
The defined semantics of :ref:`MPIX_Comm_agree` are maintained when *comm*
is revoked, or when the group of *comm* contains failed MPI processes.
In particular, :ref:`MPIX_Comm_agree` is a collective operation, even
when *comm* is revoked.

WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR
------------------------------------------

When the communicator is an inter-communicator, the value of *flag* is
a *bitwise AND* operation over the values contributed by the remote
group.

When an error of class MPIX_ERR_PROC_FAILED is raised, it is consistently
raised at all MPI processes in the group(s) of *comm*, that is, both
the local and remote groups of the inter-communicator.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPIX_Comm_is_revoked`
   * :ref:`MPIX_Comm_ack_failed`
