.. _mpi_file_set_atomicity:


MPI_File_set_atomicity
======================

.. include_body

:ref:`MPI_File_set_atomicity` |mdash| Sets consistency semantics for data-access
operations (collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_set_atomicity(MPI_File fh, int flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_SET_ATOMICITY(FH, FLAG, IERROR)
   	INTEGER	FH, FLAG, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_set_atomicity(fh, flag, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	LOGICAL, INTENT(IN) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``flag``: **true** to enable atomic mode, **false** to enable nonatomic mode (boolean).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The consistency semantics for data-access operations using the set of
file handles created by one collective :ref:`MPI_File_open` is set by
collectively calling :ref:`MPI_File_set_atomicity`. All processes in the group
must pass identical values for *fh* and *flag.* If *flag* is *true,*
atomic mode is set; if *flag* is *false,* nonatomic mode is set.

The default value on a call to :ref:`MPI_File_open` in Open MPI is *true* for
jobs running on more than one node, *false* for jobs running on a single
SMP. For more information, see the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
