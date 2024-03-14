.. _label-building-ompi-cli-options-required-support-libraries:

CLI Options for required support libraries
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following ``configure`` command line options are for Open MPI's
:ref:`required support libraries
<label-install-required-support-libraries>`

* ``--with-hwloc[=VALUE]``:
* ``--with-libevent[=VALUE]``:
* ``--with-pmix[=VALUE]``:
* ``--with-prrte[=VALUE]``: These four options specify where to find
  the headers and libraries for the `Hwloc
  <https://www.open-mpi.org/projects/hwloc/>`_, `Libevent
  <https://libevent.org/>`_, `PMIx <https://openpmix.github.io/>`_,
  and `PRRTE <https://github.com/openpmix/prrte>`_ libraries,
  respectively.  The following ``VALUE``\s are permitted:

  * ``external``: Use an external installation (rely on default
    compiler and linker paths to find it).  ``configure`` will abort
    if it cannot find suitable header files and libraries.
  * ``internal``: Use Open MPI's internal/bundled copy.
  * No value specified: Try the ``external`` behavior.  If that fails,
    fall back to ``internal`` behavior.  *This is the default behavior.*
  * ``DIR``: Specify the location of a specific installation to use.
    ``configure`` will abort if it cannot find suitable header files
    and libraries under ``DIR``.

* ``--with-hwloc-libdir=LIBDIR``:
* ``--with-libevent-libdir=LIBDIR``:
* ``--with-prrte-libdir=LIBDIR``:
* ``--with-pmix-libdir=LIBDIR``:
  :ref:`See the configure CLI
  options conventions <building-ompi-cli-options-conventions-label>`
  for a description of these four options.

* ``--with-valgrind[=DIR]``:
  Directory where the valgrind software is installed.  If Open MPI
  finds Valgrind's header files, it will include additional support
  for Valgrind's memory-checking debugger.

  Valgrind support is disabled by default in Open MPI.  Enabling
  Valgrind support will eliminate a lot of false positives when
  running Valgrind on MPI applications.  There is a minor performance
  penalty for enabling this option.
