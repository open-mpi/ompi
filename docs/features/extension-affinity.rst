.. _ompi-features-extension-affinity-label:

Affinity extension
==================

Overview
--------

The ``affinity`` extension provides a single Open MPI-specific
function, :ref:`OMPI_Affinity_str(3) <ompi_affinity_str>`.  Given a
format selector, it fills in three human-readable strings describing:

* where Open MPI bound the calling process at launch time (or that it
  was not bound);
* where the process is *currently* bound (or that it is unbound); and
* which processors exist on the local host.

The strings can be requested either as resource descriptions (for
example, ``socket 0, core 0``) or as an ASCII-art layout of the
machine (for example, ``[. B][. .]``).  This is an Open
MPI-specific convenience API for applications that want to introspect
their own binding; it is not part of the MPI standard.

When it is built
----------------

The ``affinity`` extension has no build-time prerequisites, so it is
built by default on every platform.  Like all extensions, it can be
excluded by configuring with ``--disable-mpi-ext``, or by naming an
explicit list that omits it (for example,
``--enable-mpi-ext=cuda,rocm``).  See :ref:`the extensions overview
<ompi-features-extensions-label>` for the full set of
``--enable-mpi-ext`` / ``--disable-mpi-ext`` options.

Availability at run time
------------------------

Because the extension has no external dependencies, it is available at
run time whenever it was compiled in.  Applications should still guard
their use of it with the preprocessor macro
``OMPI_HAVE_MPI_EXT_AFFINITY``, which ``<mpi-ext.h>`` defines to ``1``
when the extension is present.  This keeps application code portable
across Open MPI builds that omitted the extension and across other MPI
implementations.

Functions
---------

* :ref:`OMPI_Affinity_str(3) <ompi_affinity_str>`
