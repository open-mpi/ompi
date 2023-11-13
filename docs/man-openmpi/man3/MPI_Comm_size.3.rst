.. _mpi_comm_size:


MPI_Comm_size
=============

.. include_body

:ref:`MPI_Comm_size` |mdash| Returns the size of the group associated with a
communicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_size(MPI_Comm comm, int *size)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_SIZE(COMM, SIZE, IERROR)
   	INTEGER	COMM, SIZE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_size(comm, size, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Number of processes in the group of comm (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function indicates the number of processes involved in a
communicator. For MPI_COMM_WORLD, it indicates the total number of
processes available. This function is equivalent to accessing the
communicator's group with :ref:`MPI_Comm_group`, computing the size using
:ref:`MPI_Group_size`, and then freeing the temporary group via :ref:`MPI_Group_free`.
If the communicator is an inter-communicator (enables communication
between two groups), this function returns the size of the local group.
To return the size of the remote group, use the :ref:`MPI_Comm_remote_size`
function.

This call is often used with :ref:`MPI_Comm_rank` to determine the amount of
concurrency available for a specific library or program. :ref:`MPI_Comm_rank`
indicates the rank of the process that calls it in the range from 0 . .
. size-1, where size is the return value of :ref:`MPI_Comm_size`.


NOTE
----

MPI_COMM_NULL is not considered a valid argument to this function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_group`
   * :ref:`MPI_Comm_rank`
   * :ref:`MPI_Comm_compare`
