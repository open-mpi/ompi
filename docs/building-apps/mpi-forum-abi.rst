.. _label-building-with-mpi-abi:

Building MPI applications using the MPI Forum ABI
=================================================

.. note:: Open MPI supports two ABIs:

          * **The Open MPI ABI:** this is the ABI that Open MPI has
            supported for multiple releases over many years.

          * **The MPI Forum ABI:** this is the official ABI as defined
            by the MPI standard.  It was not supported in Open MPI
            until v6.0.0.

          These two ABIs are different and not interchangeable.

This section describes the **MPI Forum ABI** support in Open MPI.

For information about the Open MPI ABI, see
:ref:`ABI compatibility to previous versions of Open MPI
<label-binary-compatibility>`.

What is the MPI Forum ABI?
--------------------------

Starting with MPI-5.0, the MPI standard defines an Application Binary
Interface (ABI) for MPI applications.  In principle, an application can
be compiled against one MPI implementation that supports the standard
MPI ABI, and later run with another MPI implementation that supports
the same standard MPI ABI.

There are a few important limitations:

* The application must be dynamically linked.
* The application must be launched with a launcher that is suitable for
  the MPI library used at run time.
* This release of Open MPI supports building C MPI applications against
  the standard MPI ABI.  It does not provide a Fortran ABI compiler
  wrapper.

The MPI standard ABI is different than Open MPI's own ABI compatibility
within an Open MPI release series.  See :ref:`Open MPI ABI
compatibility <label-binary-compatibility>` for information about
running applications that were compiled with an earlier Open MPI
release.

The MPI-5.0 standard ABI version is 1.0.  Open MPI installs the
standard ABI library as ``libmpi_abi`` when standard ABI support is
enabled.

How to build an application using the MPI ABI
---------------------------------------------

Use the ``mpicc_abi`` compiler wrapper to compile and link C MPI
applications against the standard MPI ABI:

.. code-block:: sh

   shell$ mpicc_abi hello.c -o hello

Then launch the application with the ``mpirun`` from the MPI
implementation that will provide ``libmpi_abi`` at run time:

.. code-block:: sh

   shell$ mpirun -np 2 ./hello

When using Open MPI's ``mpicc_abi``, the wrapper adds the standard ABI
header directory and links against ``libmpi_abi``.  You can inspect the
wrapper-provided flags with:

.. code-block:: sh

   shell$ mpicc_abi --showme:compile
   shell$ mpicc_abi --showme:link

The standard ABI ``mpi.h`` is installed under
``$prefix/include/standard_abi``.  The normal Open MPI ``mpi.h`` remains
installed under ``$prefix/include`` and is used by the normal ``mpicc``
wrapper.

.. note:: If Open MPI is configured with ``--disable-standard-abi``,
          ``libmpi_abi`` is not built and the installation cannot be
          used as the run-time MPI library for standard MPI ABI
          applications.

Checking ABI support at run time
--------------------------------

Applications can call :ref:`MPI_Abi_get_version` to query the ABI
version provided by the MPI library at run time.  When running against
Open MPI's standard ABI library, this routine returns ABI version 1.0.

The normal Open MPI ``libmpi`` library also provides the
``MPI_Abi_get_version`` routine, but it returns ``-1`` for both the
major and minor version numbers because the normal ``libmpi`` library
uses Open MPI's own ABI, not the MPI standard ABI.

Fortran support
---------------

This release does not provide a ``mpifort_abi`` compiler wrapper, and
does not provide ``libmpifort_abi``.

Open MPI does provide the MPI ABI query/helper routines documented in
the MPI API man pages, including their Fortran bindings where the
normal Open MPI Fortran bindings are available.  Those routines do not
imply that this release provides a complete Fortran standard ABI build
path.

Mixing Open MPI's ABI and the MPI standard ABI
----------------------------------------------

The Open MPI ABI and the MPI Forum ABI are separate binary interfaces.

.. danger:: Do not mix object files or libraries that use both MPI
            ABIs in a single executable.

For example, the following cases are **not** supported:

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

Use *one* MPI ABI consistently for every object file that calls MPI:

* For a C MPI application using the MPI-5.0 standard ABI, you can
  compile and link all MPI-using C objects with ``mpicc`` *or*
  ``mpicc_abi``.
* For other MPI applications, including mixed C and Fortran
  applications, compile C objects with ``mpicc`` and Fortran objects
  with ``mpifort``.
