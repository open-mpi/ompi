.. _mpi_info_create_env:


MPI_Info_create_env
===================

.. include_body

:ref:`MPI_Info_create_env` - Creates a new info object with the same construction as :ref:`MPI_INFO_ENV` as created during :ref:`MPI_Init`  or :ref:`MPI_Init_thread` when the same arguments
are used.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Info_create_env(int argc, char *argv[], MPI_Info *info)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INFO_CREATE_ENV(INFO, IERROR)
   	INTEGER	INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Info_create_env(info, ierror)
   	TYPE(MPI_Info), INTENT(OUT) :: info
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


OUTPUT PARAMETERS
-----------------
* ``info``: Info object created (handle).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_create_env` creates a new info object with the same construction as :ref:`MPI_INFO_ENV` as created during :ref:`MPI_Init` or :ref:`MPI_Init_thread` when the same arguments are used.

Note
----

:ref:`MPI_Info_create_env` is one of the few functions that can be called
before :ref:`MPI_Init` and after :ref:`MPI_Finalize`.

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
   :ref:`MPI_Info_delete` :ref:`MPI_Info_dup` :ref:`MPI_Info_free` :ref:`MPI_Info_get` :ref:`MPI_Info_set`
