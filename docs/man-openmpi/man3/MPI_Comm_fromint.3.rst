.. _mpi_comm_fromint:


MPI_Comm_fromint
================

.. include_body

:ref:`MPI_Comm_fromint`, :ref:`MPI_Comm_toint`,
:ref:`MPI_Errhandler_fromint`, :ref:`MPI_Errhandler_toint`,
:ref:`MPI_File_fromint`, :ref:`MPI_File_toint`,
:ref:`MPI_Group_fromint`, :ref:`MPI_Group_toint`,
:ref:`MPI_Info_fromint`, :ref:`MPI_Info_toint`,
:ref:`MPI_Message_fromint`, :ref:`MPI_Message_toint`,
:ref:`MPI_Op_fromint`, :ref:`MPI_Op_toint`,
:ref:`MPI_Request_fromint`, :ref:`MPI_Request_toint`,
:ref:`MPI_Session_fromint`, :ref:`MPI_Session_toint`,
:ref:`MPI_Status_fromint`, :ref:`MPI_Status_toint`,
:ref:`MPI_Type_fromint`, :ref:`MPI_Type_toint`,
:ref:`MPI_Win_fromint`, :ref:`MPI_Win_toint` - Translates a C integer to the appropriate C handle, or vice versa.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Comm_fromint, MPI_Comm_toint
.. mpi-bindings: MPI_Errhandler_fromint, MPI_Errhandler_toint
.. mpi-bindings: MPI_File_fromint, MPI_File_toint
.. mpi-bindings: MPI_Group_fromint, MPI_Group_toint
.. mpi-bindings: MPI_Info_fromint, MPI_Info_toint
.. mpi-bindings: MPI_Message_fromint, MPI_Message_toint
.. mpi-bindings: MPI_Op_fromint, MPI_Op_toint
.. mpi-bindings: MPI_Request_fromint, MPI_Request_toint
.. mpi-bindings: MPI_Session_fromint, MPI_Session_toint
.. mpi-bindings: MPI_Type_fromint, MPI_Type_toint
.. mpi-bindings: MPI_Win_fromint, MPI_Win_toint

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_fromint.rst

DESCRIPTION
-----------

The *fromint* function translates a C integer to the appropriate C handle.
Only an integer obtained from a previous call to :ref:`MPI_Comm_fromint`, etc.
may be passed to this function.  It is erroneous to pass to this function an integer associated
with a handle that has been freed, disconnected, or aborted (or that was derived from a session that has been finalized).


NOTES
-----

These functions are only available in C; they are not available in any
of the Fortran MPI interfaces.

These functions do not return an error value.
