.. _mpi_comm_delete_attr:

MPI_Comm_delete_attr
====================

.. include_body

:ref:`MPI_Comm_delete_attr` - Deletes attribute value associated with a
key.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_DELETE_ATTR(COMM, COMM_KEYVAL, IERROR)
       INTEGER COMM, COMM_KEYVAL, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Comm_delete_attr(comm, comm_keyval, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: comm_keyval
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input/Output Parameter
^^^^^^^^^^^^^^^^^^^^^^

-  ``comm`` : Communicator from which the attribute is deleted (handle).

Input Parameter
---------------

-  ``comm_keyval`` : Key value (integer).

Output Parameter
----------------

-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Comm_delete_attr` deletes an attribute from cache by key. This
function invokes the attribute delete function ``delete_fn`` specified
when the ``comm_keyval`` was created. The call will fail if the
``delete_fn`` function returns an error code other than MPI_SUCCESS.``
Whenever a communicator is replicated using the function
:ref:`MPI_Comm_dup`, all callback copy functions for attributes that are
currently set are invoked (in arbitrary order). Whenever a communicator
is deleted using the function :ref:`MPI_Comm_free`, all callback delete
functions for attributes that are currently set are invoked. This
function is the same as :ref:`MPI_Attr_delete` but is needed to match the
``comm``\ unicator-specific functions introduced in the MPI-2 standard.
The use of :ref:`MPI_Attr_delete` is deprecated.

Notes
-----

Note that it is not defined by the MPI standard what happens if the
``delete_fn`` callback invokes other MPI functions. In Open MPI, it is
not valid for ``delete_fn`` callbacks (or any of their children) to add
or delete attributes on the same object on which the ``delete_fn``
callback is being invoked.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
