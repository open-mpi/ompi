.. _label-install-packagers:

Advice for packagers
====================

.. _label-install-packagers-do-not-use-internal:

Do not use Open MPI's internal dependent libraries
--------------------------------------------------

The Open MPI community **strongly** suggests that binary Open MPI
packages should *not* include Hwloc, Libevent, PMIx, or PRRTE.
:ref:`Although several of these libraries are required by Open MPI
<label-install-required-support-libraries>` (and are therefore bundled
in the Open MPI source code distribution for end-user convenience),
binary Open MPI packages should limit themselves solely to Open MPI
artifacts.  Specifically: ensure to configure and build Open MPI
against external installations of these required packages.

Packagers may therefore wish to configure Open MPI with something like
the following:

.. code-block:: sh

   # Install Sphinx so that Open MPI can re-build its docs with the
   # installed PRRTE's docs

   virtualalenv venv
   . ./venv/bin/activate
   pip install docs/requirements.txt

   ./configure --with-libevent=external --with-hwloc=external \
       --with-pmix=external --with-prrte=external ...

.. important:: Note the installation of the Sphinx tool so that Open
               MPI can re-build its documentation with the external
               PRRTE's documentation.

               Failure to do this will mean Open MPI's documentation
               will be correct for the version of PRRTE that is
               bundled in the Open MPI distribution, but may not be
               entirely correct for the version of PRRTE that you are
               building against.

The ``external`` keywords will force Open MPI's ``configure`` to
ignore all the bundled libraries and only look for external versions
of these support libraries.  This also has the benefit of causing
``configure`` to fail if it cannot find the required support libraries
outside of the Open MPI source tree |mdash| a good sanity check to
ensure that your package is correctly relying on the
independently-built and installed versions.

:ref:`See this section
<label-building-ompi-cli-options-required-support-libraries>` for more
information about the required support library ``--with-FOO`` command
line options.

Have Sphinx installed
---------------------

Since you should be (will be) installing Open MPI against an external
PRRTE and PMIx, you should have `Sphinx
<https://www.sphinx-doc.org/>`_ installed before running Open MPI's
``configure`` script.

This will allow Open MPI to (re-)build its documentation according to
the PMIx and PRRTE that you are building against.

To be clear: the Open MPI distribution tarball comes with pre-built
documentation |mdash| rendered in HTML and nroff |mdash| that is
suitable for the versions of PRRTE and PMIx that are bundled in that
tarball.

However, if you are building Open MPI against not-bundled versions of
PRRTE / PMIx (as all packagers should be), Open MPI needs to re-build
its documentation with specific information from those external PRRTE
/ PMIx installs.  For that, you need to have Sphinx installed before
running Open MPI's ``configure`` script.


.. _label-install-packagers-dso-or-not:

Components ("plugins"): static or DSO?
--------------------------------------

Open MPI contains a large number of components (sometimes called
"plugins") to effect different types of functionality in MPI.  For
example, some components effect Open MPI's networking functionality:
they may link against specialized libraries to provide
highly-optimized network access.

Open MPI can build its components as Dynamic Shared Objects (DSOs) or
statically included in core libraries (regardless of whether those
libraries are built as shared or static libraries).

.. note:: As of Open MPI |ompi_ver|, ``configure``'s global default is
          to build all components as static (i.e., part of the Open
          MPI core libraries, not as DSOs).  Prior to Open MPI v5.0.0,
          the global default behavior was to build most components as
          DSOs.

Why build components as DSOs?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are advantages to building components as DSOs:

* Open MPI's core libraries |mdash| and therefore MPI applications
  |mdash| will have very few dependencies.  For example, if you build
  Open MPI with support for a specific network stack, the libraries in
  that network stack will be dependencies of the DSOs, not Open MPI's
  core libraries (or MPI applications).

* Removing Open MPI functionality that you do not want is as simple as
  removing a DSO from ``$libdir/open-mpi``.

Why build components as part of Open MPI's core libraries?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The biggest advantage to building the components as part of Open MPI's
core libraries is when running at (very) large scales when Open MPI is
installed on a network filesystem (vs. being installed on a local
filesystem).

For example, consider launching a single MPI process on each of 1,000
nodes.  In this scenario, the following is accessed from the network
filesystem:

#. The MPI application
#. The core Open MPI libraries and their dependencies (e.g.,
   ``libmpi``)

   * Depending on your configuration, this is probably on the order of
     10-20 library files.

#. All DSO component files and their dependencies

   * Depending on your configuration, this can be 200+ component
     files.

If all components are physically located in the libraries, then the
third step loads zero DSO component files.  When using a networked
filesystem while launching at scale, this can translate to large
performance savings.

.. note:: If not using a networked filesystem, or if not launching at
          scale, loading a large number of DSO files may not consume a
          noticeable amount of time during MPI process launch.  Put
          simply: loading DSOs as indvidual files generally only
          matters when using a networked filesystem while launching at
          scale.

Direct controls for building components as DSOs or not
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open MPI |ompi_ver| has two ``configure``-time defaults regarding the
treatment of components that may be of interest to packagers:

#. Open MPI's libraries default to building as shared libraries
   (vs. static libraries).  For example, on Linux, Open MPI will
   default to building ``libmpi.so`` (vs. ``libmpi.a``).

   .. note:: See the descriptions of ``--disable-shared`` and
             ``--enable-static`` :ref:`in this section
             <label-building-installation-cli-options>` for more
             details about how to change this default.

             Also be sure to :ref:`see this warning about building
             static apps <label-building-fully-static-apps>`.

#. Open MPI will default to including its components in its libraries
   (as opposed to being compiled as dynamic shared objects, or DSOs).
   For example, ``libmpi.so`` on Linux systems will contain the UCX
   PML component, instead of the UCX PML being compiled into
   ``mca_pml_ucx.so`` and dynamically opened at run time via
   ``dlopen(3)``.

   .. note:: See the descriptions of ``--enable-mca-dso`` and
             ``--enable-mca-static`` :ref:`in this section
             <label-building-installation-cli-options>` for more
             details about how to change this defaults.

A side effect of these two defaults is that all the components
included in the Open MPI libraries will bring their dependencies with
them.  For example (on Linux), if the XYZ PML component in the MPI
layer requires ``libXYZ.so``, then these defaults mean that
``libmpi.so`` will depend on ``libXYZ.so``.  This dependency will
likely be telegraphed into the Open MPI binary package that includes
``libmpi.so``.

Conversely, if the XYZ PML component was built as a DSO, then |mdash|
assuming no other parts of Open MPI require ``libXYZ.so`` |mdash|
``libmpi.so`` would *not* be dependent on ``libXYZ.so``.  Instead, the
``mca_pml_xyz.so`` DSO would have the dependency upon ``libXYZ.so``.

Packagers can use these facts to potentially create multiple binary
Open MPI packages, each with different dependencies by, for example,
using ``--enable-mca-dso`` to selectively build some components as
DSOs and leave the others included in their respective Open MPI
libraries.

.. code:: sh

   # Build all the "accelerator" components as DSOs (all other
   # components will default to being built in their respective
   # libraries)
   shell$ ./configure --enable-mca-dso=accelerator ...

This allows packaging ``$libdir`` as part of the "main" Open MPI
binary package, but then packaging
``$libdir/openmpi/mca_accelerator_*.so`` as sub-packages.  These
sub-packages may inherit dependencies on the CUDA and/or ROCM
packages, for example.  User can always install the "main" Open MPI
binary package, and can install the additional "accelerator" Open MPI
binary sub-package if they actually have accelerator hardware
installed (which will cause the installation of additional
dependencies).

.. _label-install-packagers-gnu-libtool-dependency-flattening:

GNU Libtool dependency flattening
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When compiling Open MPI's components statically as part of Open MPI's
core libraries, `GNU Libtool <https://www.gnu.org/software/libtool/>`_
|mdash| which is used as part of Open MPI's build system |mdash| will
attempt to "flatten" dependencies.

For example, the :ref:`ompi_info(1) <man1-ompi_info>` command links
against the Open MPI core library ``libopen-pal``.  This library will
have dependencies on various HPC-class network stack libraries. For
simplicity, the discussion below assumes that Open MPI was built with
support for `Libfabric <https://libfabric.org/>`_ and `UCX
<https://openucx.org/>`_, and therefore ``libopen-pal`` has direct
dependencies on ``libfabric`` and ``libucx``.

In this scenario, GNU Libtool will automatically attempt to "flatten"
these dependencies by linking :ref:`ompi_info(1) <man1-ompi_info>`
directly to ``libfabric`` and ``libucx`` (vs. letting ``libopen-pal``
pull the dependencies in at run time).

* In some environments (e.g., Ubuntu 22.04), the compiler and/or
  linker will automatically utilize the linker CLI flag
  ``-Wl,--as-needed``, which will effectively cause these dependencies
  to *not* be flattened: :ref:`ompi_info(1) <man1-ompi_info>` will
  *not* have a direct dependencies on either ``libfabric`` or
  ``libucx``.

* In other environments (e.g., Fedora 38), the compiler and linker
  will *not* utilize the ``-Wl,--as-needed`` linker CLI flag.  As
  such, :ref:`ompi_info(1) <man1-ompi_info>` will show direct
  dependencies on ``libfabric`` and ``libucx``.

**Just to be clear:** these flattened dependencies *are not a
problem*.  Open MPI will function correctly with or without the
flattened dependencies.  There is no performance impact associated
with having |mdash| or not having |mdash| the flattened dependencies.
We mention this situation here in the documentation simply because it
surprised some Open MPI downstream package managers to see that
:ref:`ompi_info(1) <man1-ompi_info>` in Open MPI |ompi_ver| had more
shared library dependencies than it did in prior Open MPI releases.

If packagers want :ref:`ompi_info(1) <man1-ompi_info>` to not have
these flattened dependencies, use either of the following mechanisms:

#. Use ``--enable-mca-dso`` to force all components to be built as
   DSOs (this was actually the default behavior before Open MPI v5.0.0).

#. Add ``LDFLAGS=-Wl,--as-needed`` to the ``configure`` command line
   when building Open MPI.

   .. note:: The Open MPI community specifically chose not to
             automatically utilize this linker flag for the following
             reasons:

             #. Having the flattened dependencies does not cause any
                correctness or performance problems.
             #. There's multiple mechanisms (see above) for users or
                packagers to change this behavior, if desired.
             #. Certain environments have chosen to have |mdash| or
                not have |mdash| this flattened dependency behavior.
                It is not Open MPI's place to override these choices.
             #. In general, Open MPI's ``configure`` script only
                utilizes compiler and linker flags if they are
                *needed*.  All other flags should be the user's /
                packager's choice.
