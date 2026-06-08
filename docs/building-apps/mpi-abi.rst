.. _label-building-with-mpi-abi:

Building MPI applications using the MPI ABI
===========================================

What is the MPI ABI?
--------------------

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

This release does not provide a ``mpifort_abi`` compiler wrapper.
Fortran MPI applications, including mixed C and Fortran applications
that use MPI from Fortran code, cannot currently be built against Open
MPI as a standard MPI ABI target.

Open MPI may still provide the MPI ABI query/helper routines documented
in the MPI API man pages, including their Fortran bindings where the
normal Open MPI Fortran bindings are available.  Those routines do not
imply that this release provides a complete Fortran standard ABI build
path.
