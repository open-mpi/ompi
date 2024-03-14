.. _mpix_comm_is_revoked:

MPIX_Comm_is_revoked
====================
.. include_body

:ref:`MPIX_Comm_is_revoked` - Test if a communicator is revoked.

This is part of the User Level Fault Mitigation :ref:`ULFM extension <ulfm-label>`.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Comm_is_revoked(MPI_Comm comm, int *flag)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   USE MPI_EXT
   ! or the older form: INCLUDE 'mpif.h'

   MPIX_COMM_IS_REVOKED(COMM, FLAG, IERROR)
        INTEGER COMM, IERROR
        LOGICAL FLAG

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   USE mpi_ext_f08

   MPIX_Comm_is_revoked(comm, flag, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        LOGICAL, INTENT(OUT) :: flag
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``flag``: *true* if the communicator is revoked.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns *flag = true* if the communicator associated with the handle
*comm* is revoked at the calling process. It returns *flag = false*
otherwise. The operation is local.

HOW CAN A COMMUNICATOR BECOME REVOKED
-------------------------------------

A communicator can become revoked when

1. the user calls the :ref:`MPIX_Comm_revoke` procedure on *comm* to revoke
   the communicator at the local process;
2. an MPI operation raised the error class MPIX_ERR_REVOKED because
   another process called the :ref:`MPIX_Comm_revoke` procedure on *comm*;
3. the communicator has the info key *mpi_error_range* set to *group* or
   *universe*, in which case the failure of any process in, respectively,
   the group of *comm* or the MPI universe caused an operation to raise
   an error of class MPIX_ERR_REVOKED.

REVOKE PROPAGATION AND ORDERING
-------------------------------

Note that in a multithreaded application, a thread calling
:ref:`MPIX_Comm_is_revoked` may return *flag = true* before the operation
that raises the first exception of class MPIX_ERR_REVOKED has completed
in a concurrent thread.

EFFECT OF A COMMUNICATOR REVOCATION
-----------------------------------

Once a communicator has been revoked at an MPI process, all subsequent
non-local operations on that communicator (with some exceptions listed
below), are considered local and must complete by raising an error of 
class MPIX_ERR_REVOKED at that MPI process.

OPERATIONS ON A REVOKED COMMUNICATOR
------------------------------------

The following operations never raise an error of class MPIX_ERR_REVOKED,
and complete with their normal semantics on a revoked communicator.

* :ref:`MPIX_Comm_agree`
* :ref:`MPIX_Comm_iagree`
* :ref:`MPIX_Comm_shrink`
* :ref:`MPIX_Comm_ishrink`

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPIX_Comm_revoke`
   * :ref:`MPIX_Comm_agree`
   * :ref:`MPIX_Comm_shrink`
