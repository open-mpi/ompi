.. _mpi_win_post:


MPI_Win_post
============

.. include_body

:ref:`MPI_Win_post` - Starts an RMA exposure epoch for the local window
associated with *win*


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_post(MPI_Group group, int assert, MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_POST(GROUP, ASSERT, WIN, IERROR)
   	INTEGER GROUP, ASSERT, WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_post(group, assert, win, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group
   	INTEGER, INTENT(IN) :: assert
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``group``: The group of origin processes (handle)
* ``assert``: Program assertion (integer)
* ``win``: Window object (handle)

OUTPUT PARAMETERS
-----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Starts an RMA exposure epoch for the local window associated with *win*.
Only the processes belonging to *group* should access the window with
RMA calls on *win* during this epoch. Each process in *group* must issue
a matching call to :ref:`MPI_Win_start`. :ref:`MPI_Win_post` does not block.

The *assert* argument is used to provide assertions on the context of

the call that may be used for various optimizations. A value of *assert*
^ 0 is always valid. The following assertion values are supported:

MPI_MODE_NOCHECK
   The matching calls to :ref:`MPI_Win_start` have not yet occurred on any
   origin processes when this call is made. This assertion must be
   present for all matching :ref:`MPI_Win_start` calls if used.

MPI_MODE_NOSTORE
   Informs that the local window was not updated by local stores or get
   calls in the preceding epoch.

MPI_MODE_NOPUT
   Informs that the local window will not be updated by put or
   accummulate calls until the ensuing wait synchronization.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Win_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Win_start` :ref:`MPI_Win_wait`
