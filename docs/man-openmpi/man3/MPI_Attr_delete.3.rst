.. _mpi_attr_delete:


MPI_Attr_delete
===============

.. include_body

:ref:`MPI_Attr_delete` - Deletes attribute value associated with a key --
use of this routine is deprecated.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Attr_delete(MPI_Comm comm, int keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   INCLUDE 'mpif.h'
   MPI_ATTR_DELETE(COMM, KEYVAL, IERROR)
   	INTEGER	COMM, KEYVAL, IERROR


INPUT PARAMETERS
----------------
* ``comm``: Communicator to which attribute is attached (handle).
* ``keyval``: The key value of the deleted attribute (integer).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2, and was
*deleted* in MPI-3. Please use :ref:`MPI_Comm_delete_attr`. This function does
not have a mpi_f08 binding.

Delete attribute from cache by key. This function invokes the attribute
delete function delete_fn specified when the keyval was created. The
call will fail if the delete_fn function returns an error code other
than MPI_SUCCESS.

Whenever a communicator is replicated using the function :ref:`MPI_Comm_dup`,
all callback copy functions for attributes that are currently set are
invoked (in arbitrary order). Whenever a communicator is deleted using
the function :ref:`MPI_Comm_free`, all callback delete functions for attributes
that are currently set are invoked.


NOTES
-----

Note that it is not defined by the MPI standard what happens if the
delete_fn callback invokes other MPI functions. In Open MPI, it is not
valid for delete_fn callbacks (or any of their children) to add or
delete attributes on the same object on which the delete_fn callback is
being invoked.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Comm_delete_attr`
