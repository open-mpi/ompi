.. _mpi_grequest_complete:

MPI_Grequest_complete
=====================

.. include_body

:ref:`MPI_Grequest_complete` - Reports that a generalized request is
complete.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Grequest_complete(MPI_Request request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GREQUEST_COMPLETE(REQUEST, IERROR)
       INTEGER REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Grequest_complete(request, ierror)
       TYPE(MPI_Request), INTENT(IN) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input/Output Parameter
----------------------

-  ``request`` : Generalized request (handle).

Output Parameter
----------------

-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Grequest_complete` informs MPI that the operations represented by
the generalized request ``request`` are complete. A call to
MPI_Wait(request, status)`` will return, and a call to
MPI_Test(request, flag, status)`` will return flag=true only after a
call to :ref:`MPI_Grequest_complete` has declared that these operations are
complete.

MPI imposes no restrictions on the code executed by the callback
functions. However, new nonblocking operations should be defined so that
the general semantic rules about MPI calls such as :ref:`MPI_Test`,
:ref:`MPI_Request_free`, or :ref:`MPI_Cancel` still hold. For example, all
these calls are supposed to be local and nonblocking. Therefore, the
callback functions ``query_fn``, ``free_fn``, or ``cancel_fn`` should
invoke blocking MPI communication calls only if the context is such that
these calls are guaranteed to return in finite time. Once :ref:`MPI_Cancel`
has been invoked, the canceled operation should complete in finite time,
regardless of the state of other processes (the operation has acquired
"local" semantics). It should either succeed or fail without
side-effects. The user should guarantee these same properties for newly
defined operations.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
