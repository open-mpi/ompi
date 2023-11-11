.. _mpi_get_address:

MPI_Get_address
===============

.. include_body

:ref:`MPI_Get_address` |mdash| Gets the address of a location in memory.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Get_address(const void *location, MPI_Aint *address)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_ADDRESS(LOCATION, ADDRESS, IERROR)
       <type> LOCATION(*)
       INTEGER(KIND=MPI_ADDRESS_KIND) ADDRESS
       INTEGER IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Get_address(location, address, ierror)
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: location
       INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: address
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``location`` : Location in caller memory (choice).

OUTPUT PARAMETERS
-----------------

* ``address`` : Address of location (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Get_address` returns the byte ``address`` of a location in
memory.

Example: Using :ref:`MPI_Get_address` for an array.

.. code-block:: fortran

   REAL :: A(100,100)
   INTEGER(MPI_ADDRESS_KIND) :: I1, I2, DIFF

   CALL MPI_GET_ADDRESS(A(1,1), I1, IERROR)
   CALL MPI_GET_ADDRESS(A(10,10), I2, IERROR)
   DIFF = I2 - I1
   ! The value of DIFF is 909*sizeofreal; the values of I1 and I2 are
   ! implementation dependent.


NOTES
-----

This routine is provided for both Fortran and C programmers and may be
useful when writing portable code. In the current release, the address
returned by this routine will be the same as that produced by the C ``&``
operator.

C users may be tempted to avoid using :ref:`MPI_Get_address` and rely on the
availability of the address operator ``&``. Note, however, that &
cast-expression is a pointer, not an address. ANSI C does not require
that the value of a pointer (or the pointer cast to int) be the absolute
address of the object pointed at although this is commonly the case.
Furthermore, referencing may not have a unique definition on machines
with a segmented address space. The use of :ref:`MPI_Get_address` to "reference" C
variables guarantees portability to such machines as well.

Current Fortran MPI codes will run unmodified and will port to any
system. However, they may fail if ``addresses`` larger than 2^32 - 1 are
used in the program. New codes should be written so that they use the
new functions. This provides compatibility with C and avoids errors on
64-bit architectures. However, such newly written codes may need to be
(slightly) rewritten to port to old Fortran 77 environments that do not
support KIND declarations.


ERRORS
------

.. include:: ./ERRORS.rst
