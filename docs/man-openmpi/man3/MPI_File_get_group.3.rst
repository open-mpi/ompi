.. _mpi_file_get_group:


MPI_File_get_group
==================

.. include_body

:ref:`MPI_File_get_group` |mdash| Returns a duplicate of the process group of a
file.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_group(MPI_File fh, MPI_Group *group)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_GROUP(FH, GROUP, IERROR)
   	INTEGER	FH, GROUP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_group(fh, group, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(MPI_Group), INTENT(OUT) :: group
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``group``: Group that opened the file (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_group` returns a duplicate of the group of the communicator
used to open the file associated with *fh.* The group is returned in
*group.* The user is responsible for freeing *group,* using
:ref:`MPI_Group_free`.


ERRORS
------

.. include:: ./ERRORS.rst
