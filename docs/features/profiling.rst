.. _open-mpi-profileing-label:

Open MPI profiling interface
============================

Open MPI |ompi_ver| supportings the "PMPI" profiling interface as
perscribed by the MPI standard for the C and Fortran bindings (*not*
the :ref:`Open MPI Java binding extensions <open-mpi-java-label>`).

Per MPI-4.0 section 15.2.1, MPI implementations must document which
bindings layer on top of each other, so that profile developers know
whether to implement the profiling interface for each binding, or
whether they can economize by implementing it only for the lowest
level routines.

In general, Open MPI's Fortran bindings are implemented on top of the
C bindings.  Hence, a profile developer who implements ``MPI_Init()``
in C will also intecept all Fortran calls to ``MPI_INIT`` regardless
of whether the user is utilizing the ``mpif.h``, ``use mpi``, or ``use
mpi_f08`` Fortran interfaces.

However, there are a handful of routines where Open MPI's Fortran
bindings are *not* a simple wrapper around the back-end C MPI binding.
Profile developers must therefore intercept the APIs listed below in
their source language interface bindings in order to receive full
profiling coverage.  Note, however, that it is only necessary for
profile developers to intercept the ``mpif.h`` binding of each of the
routines listed below; the ``use mpi`` and ``use mpi_f08`` bindings
will ultimately invoke the ``mpif.h`` binding.

* APIs that contain function pointer parameters:

  .. admonition:: Rationale
     :class: Hint

     In order for Open MPI to invoke callbacks through the function
     pointer with the proper language-specific calling conventions, it
     must know the source language from which the function pointer was
     passed.

  * :ref:`MPI_COMM_CREATE_KEYVAL`
  * :ref:`MPI_COMM_CREATE_ERRHANDLER`
  * :ref:`MPI_ERRHANDLER_CREATE`

    .. warning:: This function was deprecated by MPI-3.0.

  * :ref:`MPI_FILE_CREATE_ERRHANDLER`
  * :ref:`MPI_GREQUEST_START`
  * :ref:`MPI_KEYVAL_CREATE`

    .. warning:: This function was deprecated by MPI-3.0.

  * :ref:`MPI_OP_CREATE`
  * :ref:`MPI_REGISTER_DATAREP`
  * :ref:`MPI_SESSION_CREATE_ERRHANDLER`
  * :ref:`MPI_TYPE_CREATE_KEYVAL`
  * :ref:`MPI_WIN_CREATE_KEYVAL`
  * :ref:`MPI_WIN_CREATE_ERRHANDLER`

* APIs dealing with MPI attributes:

  .. admonition:: Rationale
     :class: Hint

     The MPI standard's treatment of attributes differs depending on
     which language / interface was used to create the attribute
     keyval, get the attribute value, or set the attribute value.

  * :ref:`MPI_COMM_GET_ATTR`
  * :ref:`MPI_COMM_SET_ATTR`
  * :ref:`MPI_TYPE_GET_ATTR`
  * :ref:`MPI_TYPE_SET_ATTR`
  * :ref:`MPI_WIN_GET_ATTR`
  * :ref:`MPI_WIN_SET_ATTR`

Note that there is no harm in intercepting *all* routines in *all*
interfaces.  Indeed, that is the most portable way to implement a
profiling interface.  Since Open MPI's Fortran bindings are |mdash|
for the most part |mdash| implemented on top of its C bindings,
profile developers can ignore all Fortran interfaces except for the
ones enumated above.
