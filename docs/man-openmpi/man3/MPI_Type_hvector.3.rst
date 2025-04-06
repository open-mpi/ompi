.. _mpi_type_hvector:


MPI_Type_hvector
================

.. include_body

:ref:`MPI_Type_hvector` |mdash| Creates a vector (strided) datatype with offset in bytes |mdash| |deprecated_favor| :ref:`MPI_Type_create_hvector`.


SYNTAX
------

.. NOTE: The bindings for this man page are not automatically
   generated from the official MPI Forum JSON/Python library because
   this function is deprecated.  Hence, this function is not included
   in the MPI Forum JSON data, and we therefore have to hard-code the
   bindings here ourselves.

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_hvector(int count, int blocklength, MPI_Aint stride,
   	MPI_Datatype oldtype, MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_TYPE_HVECTOR(COUNT, BLOCKLENGTH, STRIDE, OLDTYPE, NEWTYPE,
   		IERROR)
   	INTEGER	COUNT, BLOCKLENGTH, STRIDE, OLDTYPE
   	INTEGER	NEWTYPE, IERROR


INPUT PARAMETERS
----------------
* ``count``: Number of blocks (nonnegative integer).
* ``blocklength``: Number of elements in each block (nonnegative integer).
* ``stride``: Number of bytes between start of each block (integer).
* ``oldtype``: Old datatype (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New datatype (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Use
:ref:`MPI_Type_create_hvector` instead.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_create_hvector`
   * :ref:`MPI_Type_vector`
