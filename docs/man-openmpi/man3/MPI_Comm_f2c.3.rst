.. _mpi_comm_f2c:


MPI_Comm_f2c
============

.. include_body

:ref:`MPI_Comm_f2c`, :ref:`MPI_Comm_c2f`,
:ref:`MPI_Errhandler_f2c`, :ref:`MPI_Errhandler_c2f`,
:ref:`MPI_File_f2c`, :ref:`MPI_File_c2f`,
:ref:`MPI_Group_f2c`, :ref:`MPI_Group_c2f`,
:ref:`MPI_Info_f2c`, :ref:`MPI_Info_c2f`,
:ref:`MPI_Message_f2c`, :ref:`MPI_Message_c2f`,
:ref:`MPI_Op_f2c`, :ref:`MPI_Op_c2f`,
:ref:`MPI_Request_f2c`, :ref:`MPI_Request_c2f`,
:ref:`MPI_Session_f2c`, :ref:`MPI_Session_c2f`,
:ref:`MPI_Status_f2c`, :ref:`MPI_Status_c2f`,
:ref:`MPI_Type_f2c`, :ref:`MPI_Type_c2f`,
:ref:`MPI_Win_f2c`, :ref:`MPI_Win_c2f` - Translates a C handle into a Fortran
handle, or vice versa.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Comm_f2c, MPI_Comm_c2f
.. mpi-bindings: MPI_Errhandler_f2c, MPI_Errhandler_c2f
.. mpi-bindings: MPI_File_f2c, MPI_File_c2f
.. mpi-bindings: MPI_Group_f2c, MPI_Group_c2f
.. mpi-bindings: MPI_Info_f2c, MPI_Info_c2f
.. mpi-bindings: MPI_Message_f2c, MPI_Message_c2f
.. mpi-bindings: MPI_Op_f2c, MPI_Op_c2f
.. mpi-bindings: MPI_Request_f2c, MPI_Request_c2f
.. mpi-bindings: MPI_Session_f2c, MPI_Session_c2f
.. mpi-bindings: MPI_Type_f2c, MPI_Type_c2f
.. mpi-bindings: MPI_Win_f2c, MPI_Win_c2f

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_f2c.rst

DESCRIPTION
-----------

Handles are passed between Fortran and C by using an explicit C wrapper
to convert Fortran handles to C handles. There is no direct access to C
handles in Fortran. The type definition MPI_Fint is provided in C for
an integer of the size that matches a Fortran *INTEGER*; usually,
MPI_Fint will be equivalent to *int*. The handle translation functions
are provided in C to convert from a Fortran handle (which is an integer)
to a C handle, and vice versa.

For example, if *comm* is a valid Fortran handle to a communicator, then
:ref:`MPI_Comm_f2c` returns a valid C handle to that same communicator; if
*comm* = MPI_COMM_NULL (Fortran value), then :ref:`MPI_Comm_f2c` returns a null
C handle; if *comm* is an invalid Fortran handle, then :ref:`MPI_Comm_f2c`
returns an invalid C handle.


NOTES
-----

These functions are only available in C; they are not available in any
of the Fortran MPI interfaces.

This function does not return an error value. Consequently, the result
of calling it before :ref:`MPI_Init` or after :ref:`MPI_Finalize` is undefined.
