.. _label-customizing-wrapper-compiler:

Customizing wrapper compiler behavior
=====================================

The PMIx wrapper compiler is driven by text files that contain,
among other things, the flags that are passed to the underlying
compiler.  The text file is generated automatically for PMIx
and is customized for the compiler set that was selected when PMIx
was configured; it is *not* recommended that users edit this
file.

However, there are cases where it may be necessary or desirable to
edit the file and add to or subtract from the flags that PMIx
selected.  The file is installed in ``$pkgdatadir``, which
defaults to ``$prefix/share/pmix/pmixcc-wrapper-data.txt``.
Several environment variables are also available for run-time
replacement of the wrapper's default values (from the text file):

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

   * - ``mpmixcc``
     - ``PMIX_CC``
     - ``PMIX_CPPFLAGS``
     - ``PMIX_CFLAGS``
     - ``PMIX_LDFLAGS``
     - ``PMIX_LIBS``
     - ``pmixcc-wrapper-data.txt``

.. caution:: Note that changing the underlying compiler may not work
   at all.

   The traditional method of using multiple different compilers
   with PMIx is to install PMIx multiple times; each
   installation should be built/installed with a different compiler.
   This is annoying, but it is beyond the scope of PMIx to be able
   to fix.

Note that the values of these fields can be directly influenced by
passing flags to PMIx's ``configure`` script.  :ref:`See this
section in the Installation guide <label-building-installation-cli-options>` for
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

* ``includedir``: Directory containing PMIx's header files.  The
  proper compiler "include" flag is prepended to this directory and
  added into the preprocessor flags.

* ``libdir``: Directory containing PMIx's library files.  The
  proper compiler "include" flag is prepended to this directory and
  added into the linker flags.
