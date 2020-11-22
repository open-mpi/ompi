.. _mpi_address:


MPI_Address
===========

.. include_body

:ref:`MPI_Address` - Gets the address of a location in memory -- use of
this routine is deprecated.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Address(void *location, MPI_Aint *address)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   INCLUDE 'mpif.h'
   MPI_ADDRESS(LOCATION, ADDRESS, IERROR)
   	<type>	LOCATION (*)
   	INTEGER	ADDRESS, IERROR


INPUT PARAMETER
---------------
* ``location``: Location in caller memory (choice).

OUTPUT PARAMETERS
-----------------
* ``address``: Address of location (integer).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Get_address` instead.

The address of a location in memory can be found by invoking this
function. Returns the (byte) address of location.

Example: Using :ref:`MPI_Address` for an array.

::

   REAL A(100,100)

|
| INTEGER I1, I2, DIFF
| CALL MPI_ADDRESS(A(1,1), I1, IERROR)
| CALL MPI_ADDRESS(A(10,10), I2, IERROR)
| DIFF = I2 - I1
| ! The value of DIFF is 909*sizeofreal; the values of I1 and I2 are
| ! implementation dependent.


NOTES
-----

This routine is provided for both Fortran and C programmers and may be
useful when writing portable code. In the current release, the address
returned by this routine will be the same as that produced by the C &
operator.

C users may be tempted to avoid using :ref:`MPI_Address` and rely on the
availability of the address operator &. Note, however, that &
cast-expression is a pointer, not an address. ANSI C does not require
that the value of a pointer (or the pointer cast to int) be the absolute
address of the object pointed at although this is commonly the case.
Furthermore, referencing may not have a unique definition on machines
with a segmented address space. The use of :ref:`MPI_Address` to "reference" C
variables guarantees portability to such machines as well.


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
   :ref:`MPI_Get_address`
