.. _mpi_bcast:

MPI_Bcast
=========

.. include_body

:ref:`MPI_Bcast`, :ref:`MPI_Ibcast`, :ref:`MPI_Bcast_init` - Broadcasts a message from the process
with rank *root* to all other processes of the group.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Bcast, MPI_Ibcast, MPI_Bcast_init

.. The following file was automatically generated
.. include:: ./bindings/mpi_bcast.rst

INPUT/OUTPUT PARAMETERS
-----------------------

* ``buffer``: Starting address of buffer (choice).
* ``count``: Number of entries in buffer (integer).
* ``datatype``: Data type of buffer (handle).
* ``root``: Rank of broadcast root (integer).
* ``comm``: Communicator (handle).
* ``info``: Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------

* ``request``: Request (handle, non-blocking and persistent only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Bcast` broadcasts a message from the process with rank root to
all processes of the group, itself included. It is called by all members
of group using the same arguments for ``comm``, ``root``. On return, the
contents of root's communication buffer has been copied to all
processes.

General, derived datatypes are allowed for datatype. The type signature
of count, datatype on any process must be equal to the type signature o
f count, datatype at the root. This implies that the amount of data sent
must be equal to the amount received, pairwise between each process and
the root. :ref:`MPI_Bcast` and all other data-movement collective routines
make this restriction. Distinct type maps between sender and receiver
are still allowed.

**Example:** Broadcast 100 ints from process 0 to every process in the
group.

.. code-block:: C

   MPI_Comm comm;
   int array[100];
   int root=0;
   //...
   MPI_Bcast( array, 100, MPI_INT, root, comm);

As in many of our sample code fragments, we assume that some of the
variables (such as comm in the example above) have been assigned
appropriate values.

WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR
------------------------------------------

When the communicator is an inter-communicator, the root process in the
first group broadcasts data to all the processes in the second group.
The first group defines the root process. That process uses MPI_ROOT
as the value of its ``root`` argument. The remaining processes use
``MPI_PROC_NULL`` as the value of their ``root`` argument. All processes
in the second group use the rank of that root process in the first group
as the value of their ``root`` argument. The receive buffer arguments of
the processes in the second group must be consistent with the send
buffer argument of the root process in the first group.

NOTES
-----

This function does not support the in-place option.

ERRORS
------
.. include:: ./ERRORS.rst
