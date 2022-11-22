.. _mpi_get_address:

MPI_Get_address
===============

.. include_body

:ref:`MPI_Get_address` - Gets the address of a location in memory.

SYNTAX
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Get_address(const void *location, MPI_Aint *address)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_ADDRESS(LOCATION, ADDRESS, IERROR)
       <type> LOCATION(*)
       INTEGER(KIND=MPI_ADDRESS_KIND) ADDRESS
       INTEGER IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Get_address(location, address, ierror)
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: location
       INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: address
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

-  ``location`` : Location in caller memory (choice).

OUTPUT PARAMETERS
-----------------

-  ``address`` : Address of location (integer).
-  ``IERROR`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Get_address` returns the byte ``address`` of a location in
memory.

Example: Using :ref:`MPI_Get_address` for an array.

.. code:: fortran

   EAL A(100,100)
       INTEGER I1, I2, DIFF
       CALL MPI_GET_ADDRESS(A(1,1), I1, IERROR)
       CALL MPI_GET_ADDRESS(A(10,10), I2, IERROR)
       DIFF = I2 - I1
   ! The value of DIFF is 909*sizeofreal; the values of I1 and I2 are
   ! implementation dependent.

NOTES
-----

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
