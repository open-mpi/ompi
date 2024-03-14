.. _mpi_comm_get_parent:


MPI_Comm_get_parent
===================

.. include_body

:ref:`MPI_Comm_get_parent` |mdash| Returns the parent intercommunicator of
current spawned process.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_get_parent(MPI_Comm *parent)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_GET_PARENT(PARENT, IERROR)
   	INTEGER	PARENT, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_get_parent(parent, ierror)
   	TYPE(MPI_Comm), INTENT(OUT) :: parent
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


OUTPUT PARAMETERS
-----------------
* ``parent``: The parent communicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

If a process was started with :ref:`MPI_Comm_spawn` or :ref:`MPI_Comm_spawn_multiple`,
:ref:`MPI_Comm_get_parent` returns the "parent" intercommunicator of the
current process. This parent intercommunicator is created implicitly
inside of :ref:`MPI_Init` and is the same intercommunicator returned by the
spawn call made in the parents.

If the process was not spawned, :ref:`MPI_Comm_get_parent` returns
MPI_COMM_NULL.

After the parent communicator is freed or disconnected,
:ref:`MPI_Comm_get_parent` returns MPI_COMM_NULL.


NOTES
-----

:ref:`MPI_Comm_get_parent` returns a handle to a single intercommunicator.
Calling :ref:`MPI_Comm_get_parent` a second time returns a handle to the same
intercommunicator. Freeing the handle with :ref:`MPI_Comm_disconnect` or
:ref:`MPI_Comm_free` will cause other references to the intercommunicator to
become invalid (dangling). Note that calling :ref:`MPI_Comm_free` on the parent
communicator is not useful.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_spawn`
   * :ref:`MPI_Comm_spawn_multiple`
