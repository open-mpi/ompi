.. _mpi_win_lock:


MPI_Win_lock
============

.. include_body

:ref:`MPI_Win_lock` |mdash| Starts an RMA access epoch locking access to a
particular rank.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_lock(int lock_type, int rank, int assert, MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_LOCK(LOCK_TYPE, RANK, ASSERT, WIN, IERROR)
   	INTEGER LOCK_TYPE, RANK, ASSERT, WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_lock(lock_type, rank, assert, win, ierror)
   	INTEGER, INTENT(IN) :: lock_type, rank, assert
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``lock_type``: Either MPI_LOCK_EXCLUSIVE or MPI_LOCK_SHARED (state).
* ``rank``: Rank of locked window (nonnegative integer).
* ``assert``: Program assertion (integer).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Starts an RMA access epoch. Locks ensure that only the windows created
by specific processes can be accessed by those processes (and by no
other processes) during that epoch.

Locks are used to protect accesses to the locked target window effected
by RMA calls issued between the lock and unlock call, and to protect
local load/store accesses to a locked local window executed between the
lock and unlock call. Accesses that are protected by an exclusive lock
will not be concurrent at the window site with other accesses to the
same window that are lock protected. Accesses that are protected by a
shared lock will not be concurrent at the window site with accesses
protected by an exclusive lock to the same window.

The *assert* argument is used to provide assertions on the context of
the call that may be used for various optimizations. (See Section 6.4.4
of the MPI-2 Standard.) A value of *assert* = 0 is always valid. The
following assertion value is supported:

MPI_MODE_NOCHECK
   No other processes will hold or attempt to acquire a conflicting lock
   while the caller holds the window lock.


NOTES
-----

In a client/server environment in which clients connect to a server and
create windows that span both the client and the server, if a client or
server that has obtained a lock on such a window and then terminates
abnormally, the server or other clients may hang in a :ref:`MPI_Win_lock` call,
failing to notice that the peer MPI job has terminated.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_unlock`
   * :ref:`MPI_Win_lock_all`
