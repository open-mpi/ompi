.. _label-binary-compatibility:

ABI compatibility to previous versions of Open MPI
==================================================

.. note:: This section describes ABI compatibility between Open MPI
          releases.  For information about the MPI standard ABI defined
          by MPI-5.0, see :ref:`Building MPI applications using the MPI
          ABI <label-building-with-mpi-abi>`.

The Open MPI |ompi_series| series maintains Application Binary
Interface (ABI) compatibility for the C MPI bindings to the last major
Open MPI release. Specifically, an application compiled with Open MPI
v4.x can be executed with Open MPI |ompi_series| without having to
recompile the application.

.. important:: This section describes Open MPI's own release-to-release
               ABI compatibility.  It does not describe the MPI-5.0
               standard ABI.

               In particular, Open MPI |ompi_series| does not provide
               the MPI-5.0 Fortran standard ABI.  There is no
               ``mpifort_abi`` compiler wrapper, ABI Fortran MPI
               module, or ABI Fortran MPI library in this release.
               Open MPI may provide the ``MPI_Abi_*`` query/helper
               routines in its normal Fortran bindings, but those
               routines do not make the normal Open MPI Fortran
               bindings MPI-5.0 standard ABI bindings.

There are however a few scenarios where an application compiled with
Open MPI v4.x might not execute correctly with Open MPI |ompi_series|.

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
    matches the interface name.  This change is likely to make Open MPI
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

Mixing Open MPI's ABI and the MPI standard ABI
----------------------------------------------

Open MPI's normal ABI and the MPI-5.0 standard ABI are separate binary
interfaces.  Do not mix object files or libraries that use both MPI
ABIs in a single executable.

For example, the following cases are not supported:

* Compiling some C source files that call MPI with ``mpicc_abi`` and
  compiling other C source files that call MPI with ``mpicc``, then
  linking them into one executable.
* Compiling C source files that call MPI with ``mpicc_abi`` and
  compiling Fortran source files that call MPI with ``mpifort``, then
  linking them into one executable.
* Linking a library whose MPI-using object files were built with
  ``mpicc_abi`` into an application whose MPI-using object files were
  built with ``mpicc`` or ``mpifort``.

These combinations are unsafe because the process-wide ``MPI_*``
symbols must resolve to one MPI library ABI, while the object files were
compiled with different binary representations for MPI handles,
constants, callback arguments, and status objects.  The result is
undefined behavior; the executable may fail to link, crash, report MPI
errors, or appear to work until an MPI object crosses the ABI boundary.

Use one MPI ABI consistently for every object file that calls MPI:

* For a C MPI application using the MPI-5.0 standard ABI, compile and
  link all MPI-using C objects with ``mpicc_abi``.
* For a normal Open MPI application, including mixed C and Fortran
  applications, compile C objects with ``mpicc`` and Fortran objects
  with ``mpifort``.
