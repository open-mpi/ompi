.. _ompi-features-extension-example-label:

Example extension
=================

Overview
--------

The ``example`` extension is **non-functional**: it exists solely as a
worked template for developers who want to create a new Open MPI
extension.  It defines a demonstration API named ``OMPI_Progress()`` in
all four MPI binding types (C, the Fortran ``mpif.h`` bindings, the
Fortran ``mpi`` module, and the Fortran ``mpi_f08`` module), showing
how each piece is wired into the publicly-available ``mpi-ext.h`` header
and the ``mpi_ext`` / ``mpi_f08_ext`` Fortran modules.

It is not intended for use by applications.  Anyone writing a real
extension should read the heavily-commented source and the
``README.md`` in the source tree
(``ompi/mpiext/example/README.md``), which walk through the required
directory layout, file naming, and ``configure.m4`` conventions.

When it is built
----------------

Unlike the other extensions, ``example`` is not built as part of a
normal build.  Because it is a developer template, it is only compiled
when it is explicitly requested at configure time:

.. code-block:: sh

   shell$ ./configure --enable-mpi-ext=example <other configure params>

See :ref:`the extensions overview <ompi-features-extensions-label>` for
the full ``--enable-mpi-ext`` syntax.

Availability at run time
------------------------

When the extension is compiled in, ``<mpi-ext.h>`` defines the
preprocessor macro ``OMPI_HAVE_MPI_EXT_EXAMPLE`` to ``1``.  Since the
extension is a demonstration only, applications have no reason to depend
on it; it is documented here for completeness because it appears in the
list of available extensions.

Functions
---------

This extension provides only a demonstration ``OMPI_Progress()`` entry
point and has no manual pages.
