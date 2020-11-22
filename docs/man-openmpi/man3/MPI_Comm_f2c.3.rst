.. _mpi_comm_f2c:


MPI_Comm_f2c
============

.. include_body

:ref:`MPI_Comm_f2c`, :ref:`MPI_Comm_c2f`, :ref:`MPI_File_f2c`, :ref:`MPI_File_c2f`, :ref:`MPI_Info_f2c`,
:ref:`MPI_Info_c2f`, :ref:`MPI_Message_f2c`, :ref:`MPI_Message_c2f`, :ref:`MPI_Op_f2c`, :ref:`MPI_Op_c2f`,
:ref:`MPI_Request_f2c`, :ref:`MPI_Request_c2f`, :ref:`MPI_Type_f2c`, :ref:`MPI_Type_c2f`,
:ref:`MPI_Win_f2c`, :ref:`MPI_Win_c2f` - Translates a C handle into a Fortran
handle, or vice versa.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   MPI_Comm MPI_Comm_f2c(MPI_Fint comm)
   MPI_Fint MPI_Comm_c2f(MPI_Comm comm)

   MPI_File MPI_File_f2c(MPI_Fint file)
   MPI_Fint MPI_File_c2f(MPI_File file)

   MPI_Group MPI_Group_f2c(MPI Fint group)
   MPI_Fint MPI_Group_c2f(MPI Group group)

   MPI_Info MPI_Info_f2c(MPI_Fint info)
   MPI_Fint MPI_Info_c2f(MPI_Info info)

   MPI_Message MPI_Message_f2c(MPI_Fint message)
   MPI_Fint MPI_Message_c2f(MPI_Message message)

   MPI_Op MPI_Op_f2c(MPI_Fint op)
   MPI_Fint MPI_Op_c2f(MPI_Op op)

   MPI_Request MPI_Request_f2c(MPI_Fint request)
   MPI_Fint MPI_Request_c2f(MPI_Request request)

   MPI_Datatype MPI_Type_f2c(MPI_Fint datatype)
   MPI_Fint MPI_Type_c2f(MPI_Datatype datatype)

   MPI_Win MPI_Win_f2c(MPI_Fint win)
   MPI_Fint MPI_Win_c2f(MPI_Win win)


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


NOTE
----

This function does not return an error value. Consequently, the result
of calling it before :ref:`MPI_Init` or after :ref:`MPI_Finalize` is undefined.
