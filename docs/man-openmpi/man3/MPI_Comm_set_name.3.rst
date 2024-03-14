.. _mpi_comm_set_name:


MPI_Comm_set_name
=================

.. include_body

:ref:`MPI_Comm_set_name` |mdash| Associates a name with a communicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_set_name(MPI_Comm comm, const char *comm_name)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_SET_NAME(COMM, COMM_NAME, IERROR)
   	INTEGER	COMM, IERROR
   	CHARACTER*(*) COMM_NAME


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_set_name(comm, comm_name, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	CHARACTER(LEN=*), INTENT(IN) :: comm_name
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``comm``: Communicator whose identifier is to be set (handle).

INPUT PARAMETER
---------------
* ``comm_name``: Character string to be used as the identifier for the communicator (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_set_name` allows a user to associate a name string with a
communicator. The character string that is passed to :ref:`MPI_Comm_set_name`
is saved inside the MPI library (so it can be freed by the caller
immediately after the call, or allocated on the stack). Leading spaces
in *name* are significant, but trailing ones are not.

:ref:`MPI_Comm_set_name` is a local (noncollective) operation, which affects
only the name of the communicator as seen in the process that made the
:ref:`MPI_Comm_set_name` call. There is no requirement that the same (or any)
name be assigned to a communicator in every process where it exists.

The length of the name that can be stored is limited to the value of
MPI_MAX_OBJECT_NAME in Fortran and MPI_MAX_OBJECT_NAME-1 in C (to allow
for the null terminator). Attempts to set names longer than this will
result in truncation of the name. MPI_MAX_OBJECT_NAME must have a value
of at least 64.


NOTES
-----

Since :ref:`MPI_Comm_set_name` is provided to help debug code, it is sensible
to give the same name to a communicator in all of the processes where it
exists, to avoid confusion.

Regarding name length, under circumstances of store exhaustion, an
attempt to set a name of any length could fail; therefore, the value of
MPI_MAX_OBJECT_NAME should be viewed only as a strict upper bound on the
name length, not a guarantee that setting names of less than this length
will always succeed.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_get_name`
