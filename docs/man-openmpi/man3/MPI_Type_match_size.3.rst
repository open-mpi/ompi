.. _mpi_type_match_size:


MPI_Type_match_size
===================

.. include_body

:ref:`MPI_Type_match_size` - Returns an MPI datatype of a given type and
size


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_match_size(int typeclass, int size,
   	MPI_Datatype *type)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_MATCH_SIZE(TYPECLASS, SIZE, TYPE, IERROR)
   	INTEGER	TYPECLASS, SIZE, TYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_match_size(typeclass, size, datatype, ierror)
   	INTEGER, INTENT(IN) :: typeclass, size
   	TYPE(MPI_Datatype), INTENT(OUT) :: datatype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``typeclass``: Generic type specifier (integer).
* ``size``: Size, in bytes, of representation (integer).

OUTPUT PARAMETERS
-----------------
* ``type``: Datatype with correct type and size (handle).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function returns an MPI datatype matching a local variable of type
(*typeclass*, *size*). The returned type is a reference (handle) to a
predefined named datatype, not a duplicate. This type cannot be freed.

The value of *typeclass* may be set to one of MPI_TYPECLASS_REAL,
MPI_TYPECLASS_INTEGER, or MPI_TYPECLASS_COMPLEX, corresponding to the
desired datatype.

MPI_type_match_size can be used to obtain a size-specific type that
matches a Fortran numeric intrinsic type: first call :ref:`MPI_Sizeof` to
compute the variable size, then call :ref:`MPI_Type_match_size` to find a
suitable datatype. In C use the sizeof builtin instead of :ref:`MPI_Sizeof`.

It is erroneous to specify a size not supported by the compiler.


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

See the MPI man page for a full list of MPI error codes.


.. seealso::
   :ref:`MPI_Sizeof` :ref:`MPI_Type_get_extent`
