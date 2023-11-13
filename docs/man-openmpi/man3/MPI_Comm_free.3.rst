.. _mpi_comm_free:


MPI_Comm_free
=============

.. include_body

:ref:`MPI_Comm_free` |mdash| Mark a communicator object for deallocation.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_free(MPI_Comm *comm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_FREE(COMM, IERROR)
   	INTEGER	COMM, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_free(comm, ierror)
   	TYPE(MPI_Comm), INTENT(INOUT) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``comm``: Communicator to be destroyed (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This operation marks the communicator object for deallocation. The
handle is set to MPI_COMM_NULL. Any pending operations that use this
communicator will complete normally; the object is actually deallocated
only if there are no other active references to it. This call applies to
intracommunicators and intercommunicators. Upon actual deallocation, the
delete callback functions for all cached attributes (see the "Caching"
section in the "Groups, Contexts, and Communicators" chapter in the
`MPI Standard <https://www.mpi-forum.org/docs/>`_) are called in
arbitrary order.


NOTES
-----

Note that it is not defined by the MPI standard what happens if the
delete_fn callback invokes other MPI functions. In Open MPI, it is not
valid for delete_fn callbacks (or any of their children) to add or
delete attributes on the same object on which the delete_fn callback is
being invoked.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_delete_attr`
