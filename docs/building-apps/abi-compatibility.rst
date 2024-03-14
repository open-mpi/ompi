.. _label-binary-compatibility:

ABI compatibility to previous versions of Open MPI
==================================================

The Open MPI |ompi_series| series maintains Application Binary Interface (ABI)
compatibility for the C MPI bindings to the last major Open MPI release. Specifically, an
application compiled with Open MPI v4.x can be executed with Open MPI
|ompi_series| without having to recompile the application.

.. important:: ABI is maintained for *most* of the Fortran MPI bindings, too |mdash| see below for additional information.

There are however a few scenarios where an application compiled with
Open MPI v4.x might not execute correctly with Open MPI 5.0.

- Fortran compilers provide varying degrees of ABI guarantees between
  their releases.  As such, Open MPI can only provide ABI guarantees
  with MPI applications that use the Fortran MPI bindings when all
  three entities |mdash| Open MPI v4.x, Open MPI |ompi_series|, and
  the MPI application |mdash| were built with exactly the same Fortran
  compiler and environment.

  If these conditions are met, Open MPI's ABI guarantees between the
  Open MPI v4.x and |ompi_series| series are in effect, with the
  exception of the following cases:

  * When using the Fortran ``mpi`` module bindings with GCC compiler
    versions prior to v4.8, non-compliant Fortran interfaces for
    ``MPI_Comm_spawn_multiple``, ``MPI_Testall``, ``MPI_Testsome``,
    ``MPI_Waitall``, and ``MPI_Waitsome`` were removed starting with
    Open MPI v5.0.0.

  * When using the Fortran ``mpi`` module with modern Fortran
    compilers (e.g., GNU Fortran >= v4.9), Open MPI v5.0.0 removed the
    names from the MPI interfaces when there is only a single
    subroutine in the interface, and that subroutine name exactly
    matches the iterface name.  This change is likely to make Open MPI
    |ompi_series|'s ``mpi`` module bindings *less* restrictive than
    Open MPI v4.x, but it *may* also have ABI implications, depending
    on your Fortran compiler.

    `See this git commit message
    <https://github.com/open-mpi/ompi/commit/f34782fe0c493963ec44bcf7dde9d94b88a7ea91>`_
    for more details.

    .. important:: This is likely a compiler-specific issue, and may
                   need to be tested in your environment.

  * When using the Fortran ``mpi_f08`` module bindings in an
    environment where a Fortran ``INTEGER`` is 8 bytes but a C ``int``
    is 4 bytes, the size of a ``Type(MPI_Status)`` was increased
    starting with Open MPI v5.0.0.

- Open MPI v5.0.0 removed support for the MPI C++ bindings. If an application
  was using the deprecated and now removed C++ bindings, it will not
  be able to compile or execute with Open MPI v5.0.0.  For details on deprecated and
  removed functions see :ref:`Removed MPI constructs
  <label-removed-mpi-constructs>` and :ref:`Deprecation warnings
  <label-deprecated-functions>`
