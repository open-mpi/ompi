.. _mpi_type_hindexed:


MPI_Type_hindexed
=================

.. include_body

:ref:`MPI_Type_hindexed` - Creates an indexed datatype with offsets in
bytes -- use of this routine is deprecated.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_hindexed(int count, int *array_of_blocklengths,
   	MPI_Aint *array_of_displacements, MPI_Datatype oldtype,
   	MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   INCLUDE 'mpif.h'
   MPI_TYPE_HINDEXED(COUNT, ARRAY_OF_BLOCKLENGTHS,
   		ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
   	INTEGER	COUNT, ARRAY_OF_BLOCKLENGTHS(*)
   	INTEGER	ARRAY_OF_DISPLACEMENTS(*), OLDTYPE, NEWTYPE
   	INTEGER	IERROR


INPUT PARAMETERS
----------------
* ``count``: Number of blocks -- also number of entries in array_of_displacements and array_of_blocklengths (integer).
* ``array_of_blocklengths``: Number of elements in each block (array of nonnegative integers).
* ``array_of_displacements``: Byte displacement of each block (C: array of *MPI_Aint*, Fortran: array of integer).
* ``oldtype``: Old datatype (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New datatype (handle).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Use
:ref:`MPI_Type_create_hindexed` instead.

The function is identical to :ref:`MPI_Type_indexed`, except that block
displacements in array_of_displacements are specified in bytes, rather
than in multiples of the oldtype extent.

Assume that oldtype has type map

::

       {(type(0), disp(0)), ..., (type(n-1), disp(n-1))},

with extent ex. Let B be the array_of_blocklength argument and D be the
array_of_displacements argument. The newly created datatype has

::

   n x S^count-1
       (i=0)        B[i]  entries:

     {(type(0), disp(0) + D[0]),...,(type(n-1), disp(n-1) + D[0]),...,
     (type(0), disp(0) + (D[0] + B[0]-1)* ex),...,
     type(n-1), disp(n-1) + (D[0]+ B[0]-1)* ex),...,
     (type(0), disp(0) + D[count-1]),...,(type(n-1), disp(n-1) + D[count-1]),...,
     (type(0), disp(0) +  D[count-1] + (B[count-1] -1)* ex),...,
     (type(n-1), disp(n-1) + D[count-1] + (B[count-1] -1)* ex)}


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
   :ref:`MPI_Type_create_hindexed` :ref:`MPI_Type_indexed`
