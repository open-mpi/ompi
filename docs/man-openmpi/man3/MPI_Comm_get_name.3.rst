.. _mpi_comm_get_name:


MPI_Comm_get_name
=================

.. include_body

:ref:`MPI_Comm_get_name` |mdash| Returns the name that was most recently
associated with a communicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_GET_NAME(COMM, COMM_NAME, RESULTLEN, IERROR)
   	INTEGER	COMM, RESULTLEN, IERROR
   	CHARACTER*(*) COMM_NAME


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_get_name(comm, comm_name, resultlen, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: comm_name
   	INTEGER, INTENT(OUT) :: resultlen
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator the name of which is to be returned (handle).

OUTPUT PARAMETER
----------------
* ``comm_name``: Name previously stored on the communicator, or an empty string if no such name exists (string).
* ``resultlen``: Length of returned name (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_get_name` returns the last name that was previously associated
with the given communicator. The name may be set and retrieved from any
language. The same name will be returned independent of the language
used. *comm_name* should be allocated so that it can hold a resulting
string of length MPI_MAX_OBJECT_NAME characters. :ref:`MPI_Comm_get_name`
returns a copy of the set name in *comm_name*.

If the user has not associated a name with a communicator, or an error
occurs, :ref:`MPI_Comm_get_name` will return an empty string (all spaces in
Fortran, "" in C). The three predefined communicators will have
predefined names associated with them. Thus, the names of
MPI_COMM_WORLD, MPI_COMM_SELF, and MPI_COMM_PARENT will have the default
of MPI_COMM_WORLD, MPI_COMM_SELF, and MPI_COMM_PARENT. The fact that the
system may have chosen to give a default name to a communicator does not
prevent the user from setting a name on the same communicator; doing
this removes the old name and assigns the new one.


NOTES
-----

It is safe simply to print the string returned by :ref:`MPI_Comm_get_name`, as
it is always a valid string even if there was no name.

Note that associating a name with a communicator has no effect on the
semantics of an MPI program, and will (necessarily) increase the store
requirement of the program, since the names must be saved. Therefore,
there is no requirement that users use these functions to associate
names with communicators. However debugging and profiling MPI
applications may be made easier if names are associated with
communicators, since the debugger or profiler should then be able to
present information in a less cryptic manner.


ERRORS
------
.. include:: ./ERRORS.rst
