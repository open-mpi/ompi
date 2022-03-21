.. _mpi_info_get_nthkey:


MPI_Info_get_nthkey
===================

.. include_body

:ref:`MPI_Info_get_nthkey` - Returns the *n*\ th defined key in *info*.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_get_nthkey(MPI_Info info, int n, char *key)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_GET_NTHKEY(INFO, N, KEY, IERROR)
   	INTEGER		INFO, N, IERROR
   	CHARACTER*(*)	KEY


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_get_nthkey(info, n, key, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, INTENT(IN) :: n
   	CHARACTER(LEN=*), INTENT(OUT) :: key
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``info``: Info object (handle).
* ``n``: Key number (integer).

OUTPUT PARAMETERS
-----------------
* ``key``: Key (string).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_get_nthkey` returns the *n*\ th defined key in *info*. Keys are
numbered 0...\ *N* - 1 where *N* is the value returned by
:ref:`MPI_Info_get_nkeys`. All keys between 0 and *N* - 1 are guaranteed to be
defined. The number of a given key does not change as long as *info* is
not modified with :ref:`MPI_Info_set` or :ref:`MPI_Info_delete`.


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
   :ref:`MPI_Info_get` :ref:`MPI_Info_get_nkeys` :ref:`MPI_Info_get_valuelen`
