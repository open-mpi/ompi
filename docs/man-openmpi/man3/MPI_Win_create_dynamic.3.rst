.. _mpi_win_create_dynamic:


MPI_Win_create_dynamic
======================

.. include_body

:ref:`MPI_Win_create_dynamic` |mdash| One-sided MPI call that returns a window
object for RMA operations.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   MPI_Win_create_dynamic(MPI_Info info, MPI_Comm comm, MPI_Win *win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_CREATE_DYNAMIC(INFO, COMM, WIN, IERROR)
   	INTEGER INFO, COMM, WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_create_dynamic(info, comm, win, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Win), INTENT(OUT) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``info``: Info argument (handle).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``win``: Window object returned by the call (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_create_dynamic` is a one-sided MPI communication collective call
executed by all processes in the group of *comm*. It returns a window
object without memory attached that can be used by these processes to
perform RMA operations.

A window created with :ref:`MPI_Win_create_dynamic` requires the
*target_disp* argument for all RMA communication functions to be the
actual address at the target.

The following info keys are supported:

no_locks
   If set to *true*, then the implementation may assume that the local
   window is never locked (by a call to :ref:`MPI_Win_lock` or
   MPI_Win_lock_all). Setting this value if only active synchronization
   may allow the implementation to enable certain optimizations.

accumulate_ordering
   By default, accumulate operations from one initiator to one target on
   the same window memory location are strictly ordered. If the info key
   accumulate_ordering is set to *none*, no ordering of accumulate
   operations guaranteed. They key can also be a comma-separated list of
   required orderings consisting of *rar*, *war*, *raw*, and *waw* for
   read-after-read, write-after-read, read-after-write, and
   write-after-write, respectively. Looser ordering constraints are
   likely to result in improved performance.

accumulate_ops
   If set to *same_op*, the implementation will assume that all
   concurrent accumulate calls to the same target address will use the
   same operation. If set to *same_op_no_op*, then the implementation
   will assume that all concurrent accumulate calls to the same target
   address will use the same operation or MPI_NO_OP. The default is
   *same_op_no_op*.


NOTES
-----

Since dynamically attaching memory to a window is a local operation, one
has to communicate the actual address at the target using
:ref:`MPI_Get_address` and some communication.

Dynamic memory does not have any *disp_unit* associated and requires
correct offset calculations with proper type handling.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_attach`
   * :ref:`MPI_Win_detach`
   * :ref:`MPI_Get_address`
