.. _mpi_type_create_hvector:


MPI_Type_create_hvector
=======================

.. include_body

:ref:`MPI_Type_create_hvector` - Creates a vector (strided) data type with
offset in bytes.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_hvector(int count, int blocklength,
   	MPI_Aint stride, MPI_Datatype oldtype, MPI_Datatype *newtype)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_CREATE_HVECTOR(COUNT, BLOCKLENGTH, STRIDE, OLDTYPE,
   	NEWTYPE, IERROR)

   	INTEGER	COUNT, BLOCKLENGTH, OLDTYPE, NEWTYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) STRIDE


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype,
   		ierror)
   	INTEGER, INTENT(IN) :: count, blocklength
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: stride
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Number of blocks (nonnegative integer).
* ``blocklength``: Number of elements in each block (nonnegative integer).
* ``stride``: Number of bytes between start of each block (integer).
* ``oldtype``: Old data type (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New data type (handle).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_hvector` creates a vector (strided) data type with offset
in bytes.

NOTE - This routine replaces :ref:`MPI_Type_hvector`, which is deprecated. See
the man page :ref:`MPI_Type_hvector` for information about that routine.


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the *STRIDE*
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax

::

        INTEGER*MPI_ADDRESS_KIND STRIDE

where MPI_ADDRESS_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_hvector`
   * :ref:`MPI_Type_vector`
