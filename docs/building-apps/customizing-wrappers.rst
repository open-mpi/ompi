.. _label-customizing-wrapper-compiler:

Customizing wrapper compiler behavior
=====================================

The Open MPI wrapper compilers are driven by text files that contain,
among other things, the flags that are passed to the underlying
compiler.  These text files are generated automatically for Open MPI
and are customized for the compiler set that was selected when Open
MPI was configured; it is *not* recommended that users edit these
files.

However, there are cases where it may be necessary or desirable to
edit these files and add to or subtract from the flags that Open MPI
selected.  These files are installed in ``$pkgdatadir``, which
defaults to ``$prefix/share/openmpi/WRAPPER_NAME-wrapper-data.txt``.
Several environment variables are also available for run-time
replacement of the wrapper's default values (from the text files):

.. note:: You may need to scroll right in the following table.

.. list-table::
   :header-rows: 1

   * - Wrapper compiler
     - Compiler
     - Preprocessor flags
     - Compiler flags
     - Linker flags
     - Linker library flags
     - Data file

   * - ``mpicc``
     - ``OMPI_CC``
     - ``OMPI_CPPFLAGS``
     - ``OMPI_CFLAGS``
     - ``OMPI_LDFLAGS``
     - ``OMPI_LIBS``
     - ``mpicc-wrapper-data.txt``

   * - ``mpic++`` and ``mpiCC``
     - ``OMPI_CXX``
     - ``OMPI_CPPFLAGS``
     - ``OMPI_CXXFLAGS``
     - ``OMPI_LDFLAGS``
     - ``OMPI_LIBS``
     - ``mpic++-wrapper-data.txt`` and ``mpiCC-wrapper-data.txt``,
       respectively

   * - ``mpifort``
     - ``OMPI_FC``
     - ``OMPI_CPPFLAGS``
     - ``OMPI_FCFLAGS``
     - ``OMPI_LDFLAGS``
     - ``OMPI_LIBS``
     - ``mpifort-wrapper-data.txt``

.. caution:: Note that changing the underlying compiler may not work
   at all.

   For example, C++ and Fortran compilers are notoriously binary
   incompatible with each other (sometimes even within multiple
   releases of the same compiler).  If you compile/install Open MPI
   with C++ compiler vX.Y.Z and then use the ``OMPI_CXX`` environment
   variable to change the ``mpicxx`` wrapper compiler to use the
   vA.B.C C++ compiler, your application code may not compile and/or
   link.  The traditional method of using multiple different compilers
   with Open MPI is to install Open MPI multiple times; each
   installation should be built/installed with a different compiler.
   This is annoying, but it is beyond the scope of Open MPI to be able
   to fix.

Note that the values of these fields can be directly influenced by
passing flags to Open MPI's ``configure`` script.  :ref:`See this
section in the Installation guide <install-wrapper-flags-label>` for
more details.

The files cited in the above table use fairly simplistic "key=value"
data formats.  The following are several fields that are likely to be
of interest to end-users:

* ``project_short``: Prefix for all environment variables.  See
  below.

* ``compiler_env``: Specifies the base name of the environment
  variable that can be used to override the wrapper's underlying
  compiler at run-time.  The full name of the environment variable is
  of the form ``<project_short>_<compiler_env>``; see table above.

* ``compiler_flags_env``: Specifies the base name of the environment
  variable that can be used to override the wrapper's compiler flags
  at run-time.  The full name of the environment variable is of the
  form ``<project_short>_<compiler_flags_env>``; see table above.

* ``compiler``: The executable name of the underlying compiler.

* ``extra_includes``: Relative to ``$installdir``, a list of directories
  to also list in the preprocessor flags to find header files.

* ``preprocessor_flags``: A list of flags passed to the preprocessor.

* ``compiler_flags``: A list of flags passed to the compiler.

* ``linker_flags``: A list of flags passed to the linker.

* ``libs``: A list of libraries passed to the linker.

* ``required_file``: If non-empty, check for the presence of this file
  before continuing.  If the file is not there, the wrapper will abort
  saying that the language is not supported.

* ``includedir``: Directory containing Open MPI's header files.  The
  proper compiler "include" flag is prepended to this directory and
  added into the preprocessor flags.

* ``libdir``: Directory containing Open MPI's library files.  The
  proper compiler "include" flag is prepended to this directory and
  added into the linker flags.

* ``module_option``: This field only appears in ``mpifort``.  It is
  the flag that the Fortran compiler requires to declare where module
  files are located.
