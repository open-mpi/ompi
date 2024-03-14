.. _mpix_comm_shrink:

MPIX_Comm_shrink
================
.. include_body

:ref:`MPIX_Comm_shrink`, :ref:`MPIX_Comm_ishrink` - Create a new communicator
that includes all processes from the parent communicator that have not failed.

This is part of the User Level Fault Mitigation :ref:`ULFM extension <ulfm-label>`.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Comm_shrink(MPI_Comm comm, MPI_Comm *newcomm)
   
   int MPIX_Comm_ishrink(MPI_Comm comm, MPI_Comm *newcomm, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   USE MPI_EXT
   ! or the older form: INCLUDE 'mpif.h'

   MPIX_COMM_SHRINK(COMM, NEWCOMM, IERROR)
        INTEGER COMM, NEWCOMM, IERROR

   MPIX_COMM_ISHRINK(COMM, NEWCOMM, REQUEST, IERROR)
        INTEGER COMM, NEWCOMM, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   USE mpi_ext_f08

   MPIX_Comm_shrink(comm, newcomm, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        TYPE(MPI_Comm), INTENT(OUT) :: newcomm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPIX_Comm_ishrink(comm, newcomm, request, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        TYPE(MPI_Comm), INTENT(OUT), ASYNCHRONOUS :: newcomm
        TYPE(MPI_Request), INTENT(OUT) :: request
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``newcomm``: Communicator (handle).
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This collective operation creates a new intra- or intercommunicator
*newcomm* from the intra- or intercommunicator *comm*, respectively, by
excluding the group of failed MPI processes as shrinkd upon during the
operation.

The groups of *newcomm* must include every MPI process that returns from
:ref:`MPIX_Comm_shrink`, and it must exclude every MPI process whose failure
caused an operation on *comm* to raise an MPI error of class
MPIX_ERR_PROC_FAILED or MPIX_ERR_PROC_FAILED_PENDING at a member of the
groups of *newcomm*, before that member initiated the shrink operation.

Said otherwise, this procedure is semantically equivalent to an
:ref:`MPI_Comm_split` operation that would succeed despite failures, where
members of the groups of *newcomm* participate with the same color and a key
equal to their rank in *comm*.

:ref:`MPIX_Comm_ishrink` is the non-blocking variant of :ref:`MPIX_Comm_shrink`.
Note that, as with :ref:`MPI_Comm_idup`, it is erroneous to use *newcomm*
before *request* has completed.

WHEN THE COMMUNICATOR IS REVOKED OR CONTAINS FAILED PROCESSES
-------------------------------------------------------------

This function never raises an error of classes MPIX_ERR_REVOKED or
MPIX_ERR_PROC_FAILED. The defined semantics of :ref:`MPIX_Comm_shrink` and
:ref:`MPIX_Comm_ishrink` are maintained when *comm* is revoked, or when the
group of *comm* contains failed MPI processes. In particular,
:ref:`MPIX_Comm_shrink` and :ref:`MPIX_Comm_ishrink` are collective operations,
even when *comm* is revoked.

The implementation will strive to detect all failures during the shrink
operation, but in certain circumpstances, the group of *newcomm* may still
contain failed MPI processes, whose failure will be detected in subsequent
MPI operations on *newcomm*.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPIX_Comm_is_revoked`
