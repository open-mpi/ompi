.. _mpi_file_close:


MPI_File_close
==============

.. include_body

:ref:`MPI_File_close` |mdash| Closes a file (collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_close(MPI_File *fh)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_CLOSE(FH, IERROR)
   	INTEGER	FH, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_close(fh, ierror)
   	TYPE(MPI_File), INTENT(INOUT) :: fh
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_close` first synchronizes file state, then closes the file
associated with *fh.* :ref:`MPI_File_close` is a collective routine. The user
is responsible for ensuring that all outstanding requests associated
with *fh* have completed before calling :ref:`MPI_File_close`.


ERRORS
------

.. include:: ./ERRORS.rst
