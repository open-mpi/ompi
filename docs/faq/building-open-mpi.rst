Building Open MPI
=================

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

How do I statically link to the libraries of Intel compiler suite?
------------------------------------------------------------------

The Intel compiler suite, by default, dynamically links its runtime libraries
against the Open MPI binaries and libraries. This can cause problems if the Intel
compiler libraries are installed in non-standard locations. For example, you might
get errors like:

.. code-block::

   error while loading shared libraries: libimf.so: cannot open shared object file:
   No such file or directory

To avoid such problems, you can pass flags to Open MPI's configure
script that instruct the Intel compiler suite to statically link its
runtime libraries with Open MPI:

.. code-block::

   shell$ ./configure CC=icc CXX=icpc FC=ifort LDFLAGS=-Wc,-static-intel ...

/////////////////////////////////////////////////////////////////////////

Why do I get errors about hwloc or libevent not found?
------------------------------------------------------

Sometimes you may see errors similar to the following when attempting
to build Open MPI:

.. code-block::

   ...
   PPFC     profile/pwin_unlock_f08.lo
   PPFC     profile/pwin_unlock_all_f08.lo
   PPFC     profile/pwin_wait_f08.lo
   FCLD     libmpi_usempif08.la
   ld: library not found for -lhwloc
   collect2: error: ld returned 1 exit status
   make``2``: *** ``libmpi_usempif08.la`` Error 1

This error can happen when a number of factors occur together:

#. If Open MPI's ``configure`` script chooses to use an "external"
   installation of `hwloc <https://www.open-mpi.org/projects/hwloc/>`_
   and/or `Libevent <https://libevent.org/>`_ (i.e., outside of Open
   MPI's source tree).
#. If Open MPI's ``configure`` script chooses C and Fortran compilers
   from different suites/installations.

Put simply: if the default search library search paths differ between
the C and Fortran compiler suites, the C linker may find a
system-installed ``libhwloc`` and/or ``libevent``, but the Fortran linker
may not.

This may tend to happen more frequently starting with Open MPI v4.0.0
on Mac OS because:

#. In v4.0.0, Open MPI's ``configure`` script was changed to "prefer"
   system-installed versions of hwloc and Libevent (vs. preferring the
   hwloc and Libevent that are bundled in the Open MPI distribution
   tarballs).
#. In MacOS, it is common for `Homebrew <https://brew.sh/>`_ or
   `MacPorts <https://www.macports.org/>`_ to install:
   * hwloc and/or Libevent
   * gcc and gfortran

For example, as of July 2019, Homebrew:

* Installs hwloc v2.0.4 under ``/usr/local``
* Installs the Gnu C and Fortran compiler suites v9.1.0 under
  ``/usr/local``.  *However*, the C compiler executable is named ``gcc-9``
  (not ``gcc``!), whereas the Fortran compiler executable is
  named ``gfortran``.

These factors, taken together, result in Open MPI's ``configure``
script deciding the following:

* The C compiler is ``gcc`` (which is the MacOS-installed C
  compiler).
* The Fortran compiler is ``gfortran`` (which is the
  Homebrew-installed Fortran compiler).
* There is a suitable system-installed hwloc in ``/usr/local``, which
  can be found -- by the C compiler/linker -- without specifying any
  additional linker search paths.

The careful reader will realize that the C and Fortran compilers are
from two entirely different installations.  Indeed, their default
library search paths are different:

* The MacOS-installed ``gcc`` will search ``/usr/local/lib`` by
  default.
* The Homebrew-installed ``gfortran`` will *not* search
  ``/usr/local/lib`` by default.

Hence, since the majority of Open MPI's source code base is in C, it
compiles/links against hwloc successfully.  But when Open MPI's
Fortran code for the ``mpi_f08`` module is compiled and linked, the
Homebrew-installed ``gfortran`` -- which does not search
``/usr/local/lib`` by default -- cannot find ``libhwloc``, and the link
fails.

There are a few different possible solutions to this issue:

#. The best solution is to always ensure that Open MPI uses a C and
   Fortran compiler from the same suite/installation.  This will
   ensure that both compilers/linkers will use the same default
   library search paths, and all behavior should be consistent.  For
   example, the following instructs Open MPI's ``configure`` script to
   use ``gcc-9`` for the C compiler, which (as of July 2019) is the
   Homebrew executable name for its installed C compiler:

   .. code-block:: sh

      shell$ ./configure CC=gcc-9 ...

      # You can be precise and specify an absolute path for the C
      # compiler, and/or also specify the Fortran compiler:
      shell$ ./configure CC=/usr/local/bin/gcc-9 FC=/usr/local/bin/gfortran ...

   Note that this will likely cause ``configure`` to *not* find the
   Homebrew-installed hwloc, and instead fall back to using the
   bundled hwloc in the Open MPI source tree.

#. Alternatively, you can simply force ``configure`` to select the
   bundled versions of hwloc and libevent, which avoids the issue
   altogether:

   .. code-block:: sh

      shell$ ./configure --with-hwloc=internal --with-libevent=internal ...

#. Finally, you can tell ``configure`` exactly where to find the
   external hwloc library.  This can have some unintended
   consequences, however, because it will prefix both the C and
   Fortran linker's default search paths with ``/usr/local/lib``:

   .. code-block:: sh

      shell$ ./configure --with-hwloc-libdir=/usr/local/lib ...

Be sure to :ref:`see this section of the Installation guide
<label-install-required-support-libraries>` for more
information about the bundled hwloc and/or Libevent
vs. system-installed versions.
