.. _mpi_grequest_start:

MPI_Grequest_start
==================

.. include_body

:ref:`MPI_Grequest_start` |mdash| Starts a generalized request and returns a
handle to it in ``request``.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Grequest_start(MPI_Grequest_query_function *query_fn,
       MPI_Grequest_free_function *free_fn,
       MPI_Grequest_cancel_function *cancel_fn, void *extra_state,
       MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GREQUEST_START(QUERY_FN, FREE_FN, CANCEL_FN, EXTRA_STATE,
       REQUEST, IERROR)
       INTEGER REQUEST, IERROR
       EXTERNAL QUERY_FN, FREE_FN, CANCEL_FN
         INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, request,
           ierror)
       PROCEDURE(MPI_Grequest_query_function) :: query_fn
       PROCEDURE(MPI_Grequest_free_function) :: free_fn
       PROCEDURE(MPI_Grequest_cancel_function) :: cancel_fn
       INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
       TYPE(MPI_Request), INTENT(OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``query_fn`` : Callback function invoked when request status is
   queried (function).
* ``free_fn`` : Callback function invoked when request is freed
   (function).
* ``cancel_fn`` : Callback function invoked when request is canceled
   (function).
* ``extra_state`` : Extra state.

OUTPUT PARAMETERS
-----------------

* ``request`` : Generalized request (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Grequest_start` starts a generalized ``request`` and returns a
handle to it in ``request``.

The syntax and meaning of the callback functions are listed below. All
callback functions are passed the ``extra_state`` argument that was
associated with the ``request`` by the starting call
:ref:`MPI_Grequest_start`. This can be used to maintain user-defined state
for the ``request``. In C, the query function is

.. code-block:: c

   typedef int MPI_Grequest_query_function(void *extra_state,
       MPI_Status *status);

In Fortran, it is

.. code-block:: fortran

   SUBROUTINE GREQUEST_QUERY_FUNCTION(EXTRA_STATE, STATUS, IERROR)
       INTEGER STATUS(MPI_STATUS_SIZE), IERROR
       INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE

The ``query_fn`` function computes the status that should be returned
for the generalized request. The status also includes information about
successful/unsuccessful cancellation of the request (result to be
returned by ``MPI_Test_cancelled``).

The ``query_fn`` function is invoked by the
``MPI_{Wait|Test}{any|some|all}`` call that completed the generalized
request associated with this callback. The callback function is also
invoked by calls to ``MPI_Request_get_status`` if the request is
complete when the call occurs. In both cases, the callback is passed a
reference to the corresponding status variable passed by the user to the
MPI call. If the user provided ``MPI_STATUS_IGNORE`` or
``MPI_STATUSES_IGNORE`` to the MPI function that causes ``query_fn`` to
be called, then MPI will pass a valid status object to ``query_fn``, and
this status will be ignored upon return of the callback function. Note
that ``query_fn`` is invoked only after ``MPI_Grequest_complete`` is
called on the request; it may be invoked several times for the same
generalized request. Note also that a call to
``MPI_{Wait|Test}{some|all}`` may cause multiple invocations of
``query_fn`` callback functions, one for each generalized request that
is completed by the MPI call. The order of these invocations is not
specified by MPI.

In C, the free function is

.. code-block:: c

   typedef int MPI_Grequest_free_function(void *extra_state);

And in Fortran, it is

.. code-block:: fortran

   SUBROUTINE GREQUEST_FREE_FUNCTION(EXTRA_STATE, IERROR)
       INTEGER IERROR
       INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE

The ``free_fn`` callback function is invoked to clean up user-allocated
resources when the generalized request is freed.

The ``free_fn`` function is invoked by the
``MPI_{Wait|Test}{any|some|all}`` call that completed the generalized
request associated with this callback. ``free_fn`` is invoked after the
call to ``query_fn`` for the same request. However, if the MPI call
completed multiple generalized requests, the order in which ``free_fn``
callback functions are invoked is not specified by MPI.

The ``free_fn`` callback is also invoked for generalized requests that
are freed by a call to ``MPI_Request_free`` (no call to
``MPI_{Wait|Test}{any|some|all}`` will occur for such a request). In
this case, the callback function will be called either in the MPI call
``MPI_Request_free(request)`` or in the MPI call
``MPI_Grequest_complete(request)``, whichever happens last. In other
words, in this case the actual freeing code is executed as soon as both
calls (``MPI_Request_free`` and ``MPI_Grequest_complete``) have
occurred. The ``request`` is not deallocated until after ``free_fn``
completes. Note that ``free_fn`` will be invoked only once per request
by a correct program.

In C, the cancel function is

.. code-block:: c

   typedef int MPI_Grequest_cancel_function(void *extra_state, int complete);

In Fortran, the cancel function is

.. code-block:: Fortran

   SUBROUTINE GREQUEST_CANCEL_FUNCTION(EXTRA_STATE, COMPLETE, IERROR)
        INTEGER IERROR
        INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE
        LOGICAL COMPLETE

The ``cancel_fn`` function is invoked to start the cancellation of a
generalized request. It is called by ``MPI_Request_cancel(request)``.
MPI passes to the callback function complete=true if
``MPI_Grequest_complete`` has already been called on the request, and
complete=false otherwise.


ERRORS
------

.. include:: ./ERRORS.rst

All callback functions return an error code. The code is passed back and
dealt with as appropriate for the error code by the MPI function that
invoked the callback function. For example, if error codes are returned,
then the error code returned by the callback function will be returned
by the MPI function that invoked the callback function. In the case of a
``MPI_{Wait|Test}any`` call that invokes both ``query_fn`` and
``free_fn``, the MPI call will return the error code returned by the
last callback, namely ``free_fn``. If one or more of the ``request``s
in a call to ``MPI_{Wait|Test}{some|all``} has failed, then the MPI call
will return ``MPI_ERR_IN_STATUS``. In such a case, if the MPI call was
passed an array of statuses, then MPI will return in each of the
statuses that correspond to a completed generalized ``request`` the
error code returned by the corresponding invocation of its ``free_fn``
callback function. However, if the MPI function was passed
``MPI_STATUSES_IGNORE``, then the individual error codes returned by
each callback function will be lost.
