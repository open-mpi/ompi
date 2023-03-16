.. _mpix_comm_get_failed:

MPIX_Comm_get_failed
====================
.. include_body

:ref:`MPIX_Comm_get_failed` - Obtain a group that lists failed processes
in a communicator.

This is part of the User Level Fault Mitigation :ref:`ULFM extension <ulfm-label>`.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Comm_get_failed(MPI_Comm comm, MPI_Group *failedgrp)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   USE MPI_EXT
   ! or the older form: INCLUDE 'mpif.h'

   MPIX_COMM_GET_FAILED(COMM, FAILEDGRP, IERROR)
        INTEGER COMM, FAILEDGRP, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   USE mpi_ext_f08

   MPIX_Comm_get_failed(comm, failedgrp, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        TYPE(MPI_Group), INTENT(OUT) :: failedgrp
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``failedgrp``: Group (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This local operation returns the group *failedgrp* of processes from the
communicator *comm* that are locally known to have failed.
The *failedgrp* can be empty, that is, equal to MPI_GROUP_EMPTY.

For any two groups obtained from calls to that routine at the same MPI
process, with the same *comm*, the intersection of the largest group with
the smallest group is MPI_IDENT to the smallest group, that is, the same
processes have the same ranks in the two groups, up to the size of the
smallest group.

PROCESS FAILURES
----------------

MPI makes no assumption about asynchronous progress of the failure detection.
A valid MPI implementation may choose to update the group of locally known
failed MPI processes only when it enters a function that must raise a fault
tolerance error.

It is possible that only the calling MPI process has detected the reported
failure. If global knowledge is necessary, MPI processes detecting failures
should call :ref:`MPIX_Comm_revoke` to enforce an error at other ranks.

WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR
------------------------------------------

When the communicator is an inter-communicator, the value of *failedgrp*
contains the members known to have failed in both the local and the remote
groups of *comm*.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPIX_Comm_revoke`
   * :ref:`MPIX_Comm_ack_failed`
