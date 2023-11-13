.. _mpi_comm_test_inter:


MPI_Comm_test_inter
===================

.. include_body

:ref:`MPI_Comm_test_inter` |mdash| Tests to see if a comm is an
intercommunicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_test_inter(MPI_Comm comm, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_TEST_INTER(COMM, FLAG, IERROR)
   	INTEGER	COMM, IERROR
   	LOGICAL	FLAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_test_inter(comm, flag, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``flag (Logical.)``:
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This local routine allows the calling process to determine the type of a
communicator. It returns true for an intercommunicator, false for an
intracommunicator.

The type of communicator also affects the value returned by three other
functions. When dealing with an intracommunicator (enables communication
within a single group), the functions listed below return the expected
values, group size, group, and rank. When dealing with an
inter-communicator, however, they return the following values:

:ref:`MPI_Comm_size`
     Returns the size of the local group.

:ref:`MPI_Comm_group`
     Returns the local group.

:ref:`MPI_Comm_rank`
     Returns the rank in the local group.

To return the remote group and remote group size of an
inter-communicator, use the :ref:`MPI_Comm_remote_group` and
:ref:`MPI_Comm_remote_size` functions.

The operation :ref:`MPI_Comm_compare` is valid for intercommunicators. Both
communicators must be either intra- or intercommunicators, or else
MPI_UNEQUAL results. Both corresponding local and remote groups must
compare correctly to get the results MPI_CONGRUENT and MPI_SIMILAR. In
particular, it is possible for MPI_SIMILAR to result because either the
local or remote groups were similar but not identical.

The following accessors provide consistent access to the remote group of
an intercommunicator: :ref:`MPI_Comm_remote_size`, :ref:`MPI_Comm_remote_group`.

The intercommunicator accessors (:ref:`MPI_Comm_test_inter`,
:ref:`MPI_Comm_remote_size`, MPI_Comm_remote_group) are all local operations.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_remote_group`
   * :ref:`MPI_Comm_remote_size`
   * :ref:`MPI_Intercomm_create`
   * :ref:`MPI_Intercomm_merge`
