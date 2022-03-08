.. _mpi_alloc_mem:


MPI_Alloc_mem
=============

.. include_body

:ref:`MPI_Alloc_mem` - Allocates a specified memory segment.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)


Fortran Syntax (see FORTRAN NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ALLOC_MEM(SIZE, INFO, BASEPTR, IERROR)
   	INTEGER INFO, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) SIZE, BASEPTR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Alloc_mem(size, info, baseptr, ierror)
   	USE, INTRINSIC :: ISO_C_BINDING, ONLY
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(C_PTR), INTENT(OUT) :: baseptr
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``size``: Size of memory segment in bytes (nonnegative integer).
* ``info``: Info argument (handle).

OUTPUT PARAMETERS
-----------------
* ``baseptr``: Pointer to beginning of memory segment allocated.
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Alloc_mem` allocates *size* bytes of memory. The starting address of
this memory is returned in the variable *baseptr*.


C NOTES
-------

The parameter *baseptr* is of type *void \** to allow passing any
pointer object for this parameter. The provided argument should be a
pointer to a pointer of arbitrary type (e.g., *void \*\**).


FORTRAN NOTES
-------------

There is no portable FORTRAN 77 syntax for using :ref:`MPI_Alloc_mem`. There is
no portable Fortran syntax for using pointers returned from
:ref:`MPI_Alloc_mem`. However, :ref:`MPI_Alloc_mem` can be used with Sun Fortran
compilers.

From FORTRAN 77, you can use the following non-standard declarations for
the SIZE and BASEPTR arguments:

::

              INCLUDE "mpif.h"
              INTEGER*MPI_ADDRESS_KIND SIZE, BASEPTR

From either FORTRAN 77 or Fortran 90, you can use "Cray pointers" for
the BASEPTR argument. Cray pointers are described further in the Fortran
User's Guide and are supported by many Fortran compilers. For example,

.. code-block:: fortran

              INCLUDE "mpif.h"
              REAL*4 A(100,100)
              POINTER (BASEPTR, A)
              INTEGER*MPI_ADDRESS_KIND SIZE

              SIZE = 4 * 100 * 100
              CALL MPI_ALLOC_MEM(SIZE,MPI_INFO_NULL,BASEPTR,IERR)

              ! use A

              CALL MPI_FREE_MEM(A, IERR)


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
   :ref:`MPI_Free_mem`
