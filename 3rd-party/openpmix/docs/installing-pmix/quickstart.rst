.. _label-quickstart-building-pmix:

Installation
============

This file is a *very* short overview of building and installing the
PMIx library.  Much more information is available in the `How-To
section on the PMIx web site <https://pmix.org/support/how-to/>`_.

Developer Builds
----------------

If you have checked out a DEVELOPER'S COPY of PMIx (i.e., you checked
out from Git), you should read the :doc:`Developer's Guide
</developers/index>` section before attempting to build PMIx.  You
must then run:

.. code-block:: sh

   shell$ git submodule update --init
   shell$ ./autogen.pl

The ``submodule update`` is required as PMIx incorporates a submodule
to support its `autoconf` logic. You can, however, omit the explicit
submodule update step `if` you cloned the Git repository with the
``--recursive`` flag.

You will need very recent versions of GNU Autoconf, Automake, and
Libtool.  If ``autogen.pl`` fails, read the :doc:`Developer's Guide
</developers/index>`.  If anything else fails, read the
:doc:`Developer's Guide </developers/index>`.  Finally, we suggest
reading the :doc:`Developer's Guide </developers/index>`.

.. note:: Developer's copies of OpenPMIx typically include a large
          performance penalty at run-time because of extra debugging
          overhead.


User Builds
-----------

Building PMIx is typically a combination of running ``configure``
and ``make``.  Execute the following commands to install the PMIx
system from within the directory at the top of the tree:

.. code-block:: sh

   shell$ ./configure --prefix=/where/to/install
   [...lots of output...]
   shell$ make all install

.. note:: This version of PMIx requires the following 3rd-party
          packages to build and operate:

          * either the `Libevent package
            <https://libevent.org/>`_ (any version
            of Libevent greater than or equal to 2.0.21 is acceptable) or
            the `libev package <https://metacpan.org/dist/EV/view/libev/ev.pod>`_
            (no minimum version has been identified).

          * the `HWLOC package
            <https://www.open-mpi.org/projects/hwloc/>`_ for providing
            topology information to both the host environment (by
            collecting local inventory for rollup) and local client
            processes. Any version of HWLOC greater than 1.10 is
            supported, although versions in the 2.x series are
            recommended.

Note that you must point ``configure`` at these packages if they are
in a non-standard location - libevent using the ``--with-libevent=<dir>``
option; libev using the ``-with-libev=<dir>`` option, and HWLOC package
using the ``--with-hwloc=<dir>`` option. In all cases,
PMIx will automatically detect these packages in standard locations
and use them unless otherwise specified using the
respective configure option.

If you need special access to install, then you can execute ``make
all`` as a user with write permissions in the build tree, and a
separate ``make install`` as a user with write permissions to the
install tree.

Compiling support for specific compilers and environments may require
additional command line flags when running ``configure``.  See the
:ref:`compiler flags <install-configure-compilers-and-flags-label>` entry
for more details.

Note that VPATH builds are fully supported.  For example:

.. code-block:: sh

   shell$ tar xf pmix-X.Y.Z.tar.gz
   shell$ cd pmix-X.Y.Z
   shell$ mkdir build
   shell$ cd build
   shell$ ../configure ...your options...
   [...lots of output...]
   shell$ make all install

Parallel builds are also supported (although some versions of ``make``,
such as GNU make, will only use the first target listed on the command
line when executable parallel builds).  For example (assume GNU make):

.. code-block:: sh

   shell$ make -j 4 all
   [...lots of output...]
   shell$ make install

Parallel make is generally only helpful in the build phase; the
installation process is mostly serial and does not benefit much from
parallel make.

``configure`` options
---------------------

There are many available options to ``configure`` (see ``./configure --help``
for a full list); a summary of the more commonly used ones follows:

* ``--prefix=<directory>``: Install PMIx into the base directory named
  ``<directory>``.  Hence, PMIx will place its executables in
  ``<directory>/bin``, its header files in ``<directory>/include``,
  its libraries in ``<directory>/lib``, etc.

* ``--disable-shared``: By default, ``libpmix`` is built as a shared
  library.  This switch disables this default; it is really only
  useful when used with ``--enable-static``.  Specifically, this
  option does *not* imply ``--enable-static``; enabling static
  libraries and disabling shared libraries are two independent
  options.

* ``--enable-static``: Build ``libpmix`` as a static library.  Note
  that this option does *not* imply ``--disable-shared``; enabling
  static libraries and disabling shared libraries are two independent
  options.  Please see the :ref:`Building Static Libraries
  <label-install-static-libraries>` section below for important
  details on building PMIx as a static library.

* ``--disable-show-load-errors-by-default``: Set the default value of
  the ``mca_base_component_show_load_errors`` MCA variable: the
  ``--enable`` form of this option sets the MCA variable to true, the
  ``--disable`` form sets the MCA variable to false.  The MCA
  ``mca_base_component_show_load_errors`` variable can still be
  overridden at run time via the usual MCA-variable-setting
  mechanisms; this configure option simply sets the default value.

  The ``--disable`` form of this option is intended for OpenPMIx
  packagers who tend to enable support for many different types of
  networks and systems in their packages.  For example, consider a
  packager who includes support for both the FOO and BAR networks in
  their PMIx package, both of which require support libraries
  (``libFOO.so`` and ``libBAR.so``).  If an end user only has BAR
  hardware, they likely only have ``libBAR.so`` available on their
  systems -- not ``libFOO.so``.  Disabling load errors by default will
  prevent the user from seeing potentially confusing warnings about
  the FOO components failing to load because ``libFOO.so`` is not
  available on their systems.

  Conversely, system administrators tend to build an OpenPMIx that is
  targeted at their specific environment, and contains few (if any)
  components that are not needed.  In such cases, they might want
  their users to be warned that the FOO network components failed to
  load (e.g., if ``libFOO.so`` was mistakenly unavailable), and thus
  some PMIx calls might unexpectedly return "not supported".

* ``--with-platform=FILE``: Load configure options for the build from
  ``FILE``.  Options on the command line that are not in ``FILE`` are
  also used.  Options on the command line and in ``FILE`` are replaced
  by what is in ``FILE``.

* ``--enable-python-bindings``:
  Build the Python bindings for PMIx. Note the following packages
  are required to be installed:

  .. code-block:: sh

     shell$ yum install Cython python3 python3-devel
     or...
     shell$ pip3 install Cython

Once OpenPMIx has been built and installed, it is safe to run ``make
clean`` and/or remove the entire build tree.

VPATH and parallel builds are fully supported.

Generally speaking, the only thing that users need to do to use OpenPMIx
is ensure that ``<prefix>/lib`` is in their ``LD_LIBRARY_PATH``.  Users may
need to ensure to set ``LD_LIBRARY_PATH`` in their shell setup files (e.g.,
``.bashrc``, ``.cshrc``) so that non-interactive SSH-based logins will
be able to find the OpenPMIx library.

.. _label-install-static-libraries:

Building Static Libraries
-------------------------

PMIx depends on a number of external libraries for critical
functionality.  Some of these libraries, such as `HWLOC
<https://www.open-mpi.org/projects/hwloc/>`_, can have dependencies on
a varying number of additional libraries (such as libpci or libudev).
While PMIx's wrapper compiler will add the correct direct dependencies
for third party packages, it will frequently not pull in the right
sub-libraries.  When linking against dyanamic library versions of
these dependencies, this is not a problem (and is preferred behavior
to avoid adding unnecessary indirect linking dependencies).  However,
this does cause problems for building entirely static versions of
PMIx.  It may be necessary in some circumstances to add these
dependencies via the ``LIBS`` environment variable (for building PMIx
binaries) or ``--with-wrapper-libs=LIBS`` for the wrapper compiler.
