.. _mpi_win_fence:


MPI_Win_fence
=============

.. include_body

:ref:`MPI_Win_fence` |mdash| Synchronizes RMA calls on a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_fence(int assert, MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_FENCE(ASSERT, WIN, IERROR)
   	INTEGER ASSERT, WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_fence(assert, win, ierror)
   	INTEGER, INTENT(IN) :: assert
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``assert``: Program assertion (integer).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_fence` synchronizes RMA calls on *win*. The call is collective on
the group of *win*. All RMA operations on *win* originating at a given
process and started before the fence call will complete at that process
before the fence call returns. They will be completed at their target
before the fence call returns at the target. RMA operations on *win*
started by a process after the fence call returns will access their
target window only after :ref:`MPI_Win_fence` has been called by the target
process.

The call completes an RMA access epoch if it was preceded by another
fence call and the local process issued RMA communication calls on *win*
between these two calls. The call completes an RMA exposure epoch if it
was preceded by another fence call and the local window was the target
of RMA accesses between these two calls. The call starts an RMA access
epoch if it is followed by another fence call and by RMA communication
calls issued between these two fence calls. The call starts an exposure
epoch if it is followed by another fence call and the local window is
the target of RMA accesses between these two fence calls. Thus, the
fence call is equivalent to calls to a subset of *post, start, complete,
wait*.

The *assert* argument is used to provide assertions on the context of

the call that may be used for various optimizations. A value of *assert*
^ 0 is always valid. The following assertion value is supported:

MPI_MODE_NOPRECEDE
   No local RMA calls have been issued before this fence. This assertion
   must be provided by all or no members of the group of the window. It
   may enable faster fence call by avoiding unnecessary synchronization.

MPI_MODE_NOSTORE
   Informs that the local window was not updated by local stores or get
   calls in the preceding epoch.

MPI_MODE_NOPUT
   Informs that the local window will not be updated by any put or
   accummulate calls in the ensuing epoch (until next fence call).

MPI_MODE_NOSUCCEED
   No local RMA calls will be issued after this fence. This assertion
   must be provided by all or no members of the group of the window. It
   may enable faster fence call by avoiding unnecessary synchronization.


NOTE
----

Calls to :ref:`MPI_Win_fence` should both precede and follow calls to put, get
or accumulate that are synchronized with fence calls.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_create`
   * :ref:`MPI_Win_start`
   * :ref:`MPI_Win_post`
   * :ref:`MPI_Win_complete`
   * :ref:`MPI_Win_wait`
