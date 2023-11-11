.. _mpi_register_datarep:


MPI_Register_datarep
====================

.. include_body

:ref:`MPI_Register_datarep` |mdash| Defines data representation.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Register_datarep(const char *datarep,
   	MPI_Datarep_conversion_function *read_conversion_fn,
   	MPI_Datarep_conversion_function *write_conversion_fn,
   	MPI_Datarep_extent_function *dtype_file_extent_fn,
   	void *extra_state)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_REGISTER_DATAREP(DATAREP, READ_CONVERSION_FN,
   	WRITE_CONVERSION_FN, DTYPE_FILE_EXTENT_FN,
   	EXTRA_STATE, IERROR)
   	CHARACTER*(*)	DATAREP
   	EXTERNAL	READ_CONVERSION_FN, WRITE_CONVERSION_FN, DTYPE_FILE_EXTENT_FN
   	INTEGER	IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND)	EXTRA_STATE


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Register_datarep(datarep, read_conversion_fn, write_conversion_fn,
   		dtype_file_extent_fn, extra_state, ierror)
   	CHARACTER(LEN=*), INTENT(IN) :: datarep
   	PROCEDURE(MPI_Datarep_conversion_function) :: read_conversion_fn
   	PROCEDURE(MPI_Datarep_conversion_function) :: write_conversion_fn
   	PROCEDURE(MPI_Datarep_extent_function) :: dtype_file_extent_fn
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``datarep``: Data representation identifier (string).
* ``read_conversion_fn``: Function invoked to convert from file representation to native representation (function).
* ``write_conversion_fn``: Function invoked to convert from native representation to file representation (function).
* ``dtype_file_extent_fn``: Function invoked to get the extent of a data type as represented in the file (function).
* ``extra_state``: Extra state.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Register_datarep` defines a data representation. It associates the
data representation's identifier (a string) with the functions that
convert from file representation to the native representation and vice
versa, with the function that gets the extent of a data type as
represented in the file, as well as with "extra state," which is used
for passing arguments. Once a data representation has been registered
using this routine, you may specify its identifier as an argument to
:ref:`MPI_File_set_view`, causing subsequent data-access operations to call the
specified conversion functions.

The call associates *read_conversion_fn*, *write_conversion_fn*, and
*dtype_file_extent_fn* with the data representation identifier
*datarep*. *datarep* can then be used as an argument to
:ref:`MPI_File_set_view`, causing subsequent data access operations to call the
conversion functions to convert all data items accessed between file
data representation and native representation. :ref:`MPI_Register_datarep` is a
local operation and only registers the data representation for the
calling MPI process. If *datarep* is already defined, an error in the
error class MPI_ERR_DUP_DATAREP is raised using the default file error
handler. The length of a data representation string is limited to the
value of MPI_MAX_DATAREP_STRING. MPI_MAX_DATAREP_STRING must have a
value of at least 64. No routines are provided to delete data
representations and free the associated resources; it is not expected
that an application will generate them in significant numbers.


NOTES
-----

The Fortran version of each MPI I/O routine includes a final argument,
IERROR, which is not defined in the PARAMETERS sections. This argument
is used to return the error status of the routine in the manner typical
for Fortran library routines.

The C version of each routine returns an error status as an integer
return value.

Error classes are found in mpi.h (for C) and mpif.h (for Fortran).


ERRORS
------

.. include:: ./ERRORS.rst
