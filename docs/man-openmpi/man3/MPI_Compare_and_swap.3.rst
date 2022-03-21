.. _mpi_compare_and_swap:


MPI_Compare_and_swap
====================

.. include_body

:ref:`MPI_Compare_and_swap` - Perform RMA compare-and-swap


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Compare_and_swap(const void *origin_addr, const void *compare_addr,
   	void *result_addr, MPI_Datatype datatype, int target_rank,
   	MPI_Aint target_disp, MPI_Win win)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMPARE_AND_SWAP(ORIGIN_ADDR, COMPARE_ADDR, RESULT_ADDR, DATATYPE, TARGET_RANK,
   	TARGET_DISP, WIN, IERROR)
   	<type> ORIGIN_ADDR, COMPARE_ADDR, RESULT_ADDR(*)
   	INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
   	INTEGER DATATYPE, TARGET_RANK, WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Compare_and_swap(origin_addr, compare_addr, result_addr, datatype,
   		target_rank, target_disp, win, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: origin_addr, compare_addr
   	TYPE(*), DIMENSION(..) :: result_addr
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(IN) :: target_rank
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``origin_addr``: Initial address of buffer (choice).
* ``compare_addr``: Initial address of compare buffer (choice).
* ``result_addr``: Initial address of result buffer (choice).
* ``datatype``: Data type of the entry in origin, result, and target buffers (handle).
* ``target_rank``: Rank of target (nonnegative integer).
* ``target_disp``: Displacement from start of window to beginning of target buffer (nonnegative integer).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function compares one element of type *datatype* in the compare
buffer *compare_addr* with the buffer at offset *target_disp* in the
target window specified by *target_rank* and *win* and replaces the
value at the target with the value in the origin buffer *origin_addr* if
the compare buffer and the target buffer are identical. The original
value at the target is returned in the buffer *result_addr*. The
parameter *datatype* must belong to one of the following categories of
predefined datatypes: C integer, Fortran integer, Logical,
Multi-language types, or Byte as specified in MPI-3 section 5.9.2 on page 176.

The origin and result buffers (*origin_addr* and *result_addr*) must be
disjoint.


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the
*TARGET_DISP* argument only for Fortran 90. FORTRAN 77 users may use the
non-portable syntax

::

        INTEGER*MPI_ADDRESS_KIND TARGET_DISP

where MPI_ADDRESS_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


NOTES
-----

It is the user's responsibility to guarantee that, when using the
accumulate functions, the target displacement argument is such that
accesses to the window are properly aligned according to the data type
arguments in the call to the :ref:`MPI_Compare_and_swap` function.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned. Note
that MPI does not guarantee that an MPI program can continue past an
error.
