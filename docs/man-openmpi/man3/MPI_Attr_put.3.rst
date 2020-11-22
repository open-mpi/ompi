.. _mpi_attr_put:


MPI_Attr_put
============

.. include_body

:ref:`MPI_Attr_put` - Stores attribute value associated with a key -- use
of this routine is deprecated.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   INCLUDE 'mpif.h'
   MPI_ATTR_PUT(COMM, KEYVAL, ATTRIBUTE_VAL, IERROR)
   	INTEGER	COMM, KEYVAL, ATTRIBUTE_VAL, IERROR


INPUT PARAMETERS
----------------
* ``comm``: Communicator to which attribute will be attached (handle).
* ``keyval``: Key value, as returned by MPI_KEYVAL_CREATE (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated as of MPI-2, and* was
*deleted in MPI-3. Please use :ref:`MPI_Comm_set_attr`. This* function does not
have a mpi_f08 binding.

:ref:`MPI_Attr_put` stores the stipulated attribute value attribute_val for
subsequent retrieval by :ref:`MPI_Attr_get`. If the value is already present,
then the outcome is as if :ref:`MPI_Attr_delete` was first called to delete the
previous value (and the callback function delete_fn was executed), and a
new value was next stored. The call is erroneous if there is no key with
value keyval; in particular MPI_KEYVAL_INVALID is an erroneous key
value. The call will fail if the delete_fn function returned an error
code other than MPI_SUCCESS.


NOTES
-----

Values of the permanent attributes MPI_TAG_UB, MPI_HOST, MPI_IO, and
MPI_WTIME_IS_GLOBAL may not be changed.

The type of the attribute value depends on whether C or Fortran is being
used. In C, an attribute value is a pointer (void \*); in Fortran, it is
a single integer (not a pointer, since Fortran has no pointers and there
are systems for which a pointer does not fit in an integer, e.g., any
32-bit address system that uses 64 bits for Fortran DOUBLE PRECISION).

If an attribute is already present, the delete function (specified when
the corresponding keyval was created) will be called.


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
   :ref:`MPI_Comm_set_attr`
