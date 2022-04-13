.. _mpi_info_set:


MPI_Info_set
============

.. include_body

:ref:`MPI_Info_set` - Adds a key/value pair to *info*.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_set(MPI_Info info, const char *key, const char *value)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_SET(INFO, KEY, VALUE, IERROR)
   	INTEGER		INFO, IERROR
   	CHARACTER*(*)	KEY, VALUE


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_set(info, key, value, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	CHARACTER(LEN=*), INTENT(IN) :: key, value
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``info``: Info object (handle).

INPUT PARAMETERS
----------------
* ``key``: Key (string).
* ``value``: Value (string).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_set` adds the (key,value) pair to *info* and overrides the value
if a value for the same key was previously set. The *key* and *value*
parameters are null-terminated strings in C. In Fortran, leading and
trailing spaces in *key* and *value* are stripped. If either *key* or
*value* is larger than the allowed maximums, the error MPI_ERR_INFO_KEY
or MPI_ERR_INFO_VALUE is raised, respectively.


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
   :ref:`MPI_Info_create` :ref:`MPI_Info_delete` :ref:`MPI_Info_dup` :ref:`MPI_Info_free` :ref:`MPI_Info_set`
