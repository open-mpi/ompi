.. _mpi_file_get_type_extent:


MPI_File_get_type_extent
========================

.. include_body

:ref:`MPI_File_get_type_extent` |mdash| Returns the extent of the data type in a
file.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype
   	datatype, MPI_Aint *extent)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_TYPE_EXTENT(FH, DATATYPE, EXTENT, IERROR)
   	INTEGER	FH, DATATYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND)	EXTENT


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_type_extent(fh, datatype, extent, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: extent
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``datatype``: Data type (handle).

OUTPUT PARAMETERS
-----------------
* ``extent``: Data type extent (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_type_extent` can be used to calculate *extent* for
*datatype* in the file. The extent is the same for all processes
accessing the file associated with *fh*. If the current view uses a
user-defined data representation, :ref:`MPI_File_get_type_extent` uses the
*dtype_file_extent_fn* callback to calculate the extent.


NOTES
-----

If the file data representation is other than "native," care must be
taken in constructing etypes and file types. Any of the data-type
constructor functions may be used; however, for those functions that
accept displacements in bytes, the displacements must be specified in
terms of their values in the file for the file data representation being
used. MPI will interpret these byte displacements as is; no scaling will
be done. The function :ref:`MPI_File_get_type_extent` can be used to calculate
the extents of data types in the file. For etypes and file types that
are portable data types, MPI will scale any displacements in the data
types to match the file data representation. Data types passed as
arguments to read/write routines specify the data layout in memory;
therefore, they must always be constructed using displacements
corresponding to displacements in memory.


ERRORS
------

.. include:: ./ERRORS.rst
